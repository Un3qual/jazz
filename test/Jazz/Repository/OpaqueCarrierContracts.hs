{-# LANGUAGE OverloadedStrings #-}

module Jazz.Repository.OpaqueCarrierContracts
  ( opaqueCarrierRecordUpdateTests,
  )
where

import Control.Exception (bracket)
import Control.Monad (filterM, forM_, unless)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Jazz.Repository.Root (findJazzPackageRoot)
import Jazz.TestHarness (NamedTest, failTest)
import System.Directory
  ( doesDirectoryExist,
    getTemporaryDirectory,
    removeFile,
  )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode)

opaqueCarrierRecordUpdateTests :: [NamedTest]
opaqueCarrierRecordUpdateTests =
  [ ( "rejects external record updates for opaque compiler carriers",
      testRejectsOpaqueCarrierRecordUpdates
    )
  ]

data RecordUpdateCase = RecordUpdateCase
  { recordUpdateLabel :: Text,
    recordUpdateField :: Text,
    recordUpdateSource :: Text
  }

testRejectsOpaqueCarrierRecordUpdates :: IO ()
testRejectsOpaqueCarrierRecordUpdates = do
  packageRootResult <- findJazzPackageRoot
  packageRoot <-
    case packageRootResult of
      Left message -> failTest message
      Right root -> pure root
  packageDatabase <- findCompilerPackageDatabase packageRoot
  packageUnit <- findCompilerPackageUnit packageRoot packageDatabase
  temporaryRoot <- getTemporaryDirectory
  forM_ recordUpdateCases $ \recordUpdateCase ->
    withTemporarySource temporaryRoot (recordUpdateSource recordUpdateCase) $ \sourcePath -> do
      (exitCode, standardOutput, standardError) <-
        readCreateProcessWithExitCode
          ( ( proc
                "ghc"
                [ "-fno-code",
                  "-fforce-recomp",
                  "-v0",
                  "-package-db",
                  packageDatabase,
                  "-package-id",
                  packageUnit,
                  sourcePath
                ]
            )
              { cwd = Just packageRoot
              }
          )
          ""
      let compilerOutput = Text.pack (standardOutput <> standardError)
      case exitCode of
        ExitSuccess ->
          failTest
            ( recordUpdateLabel recordUpdateCase
                <> ": external record update unexpectedly compiled"
            )
        ExitFailure _ -> do
          unless ("record field" `Text.isInfixOf` compilerOutput) $
            failTest
              ( recordUpdateLabel recordUpdateCase
                  <> ": compilation failed for an unrelated reason: "
                  <> compilerOutput
              )
          let fieldName = recordUpdateField recordUpdateCase
          unless (fieldName `Text.isInfixOf` compilerOutput) $
            failTest
              ( recordUpdateLabel recordUpdateCase
                  <> ": compiler output did not reject field '"
                  <> fieldName
                  <> "': "
                  <> compilerOutput
              )

findCompilerPackageUnit :: FilePath -> FilePath -> IO String
findCompilerPackageUnit packageRoot packageDatabase = do
  (exitCode, standardOutput, standardError) <-
    readCreateProcessWithExitCode
      ( ( proc
            "ghc-pkg"
            [ "--package-db",
              packageDatabase,
              "find-module",
              "Jazz.Compiler.ModuleGraph",
              "--simple-output"
            ]
        )
          { cwd = Just packageRoot
          }
      )
      ""
  case (exitCode, words standardOutput) of
    (ExitSuccess, [packageName]) ->
      resolveCompilerPackageUnit packageRoot packageDatabase packageName
    _ ->
      failTest
        ( "could not locate the built Jazz compiler package: "
            <> Text.pack (standardOutput <> standardError)
        )

resolveCompilerPackageUnit :: FilePath -> FilePath -> String -> IO String
resolveCompilerPackageUnit packageRoot packageDatabase packageName = do
  (exitCode, standardOutput, standardError) <-
    readCreateProcessWithExitCode
      ( ( proc
            "ghc-pkg"
            [ "--package-db",
              packageDatabase,
              "field",
              packageName,
              "id",
              "--simple-output"
            ]
        )
          { cwd = Just packageRoot
          }
      )
      ""
  case (exitCode, words standardOutput) of
    (ExitSuccess, [packageUnit]) -> pure packageUnit
    _ ->
      failTest
        ( "could not resolve the Jazz compiler package unit: "
            <> Text.pack (standardOutput <> standardError)
        )

findCompilerPackageDatabase :: FilePath -> IO FilePath
findCompilerPackageDatabase packageRoot = do
  (exitCode, standardOutput, standardError) <-
    readCreateProcessWithExitCode
      ((proc "ghc" ["--numeric-version"]) {cwd = Just packageRoot})
      ""
  case (exitCode, words standardOutput) of
    (ExitSuccess, [compilerVersion]) -> do
      let candidates =
            [ packageRoot </> "dist-newstyle" </> "packagedb" </> ("ghc-" <> compilerVersion),
              packageRoot </> "dist" </> "package.conf.inplace"
            ]
      databases <- filterM doesDirectoryExist candidates
      case databases of
        packageDatabase : _ -> pure packageDatabase
        [] ->
          failTest
            ( "could not locate the compiler package database in "
                <> Text.pack (show candidates)
            )
    _ ->
      failTest
        ( "could not identify the active GHC version: "
            <> Text.pack (standardOutput <> standardError)
        )

withTemporarySource :: FilePath -> Text -> (FilePath -> IO result) -> IO result
withTemporarySource temporaryRoot source action =
  bracket createSource removeFile $ \sourcePath -> do
    TextIO.writeFile sourcePath source
    action sourcePath
  where
    createSource = do
      (sourcePath, handle) <- openTempFile temporaryRoot "jazz-opaque-record-update.hs"
      hClose handle
      pure sourcePath

recordUpdateCases :: [RecordUpdateCase]
recordUpdateCases =
  [ externalRecordUpdateCase
      "CoreProgram entry opacity"
      ["{-# LANGUAGE DataKinds #-}"]
      [ "import Jazz.Compiler.AST (CorePhase (Resolved))",
        "import qualified Jazz.Compiler.ModuleGraph as Graph"
      ]
      "Graph.CoreProgram 'Resolved"
      "Graph.coreProgramEntry"
      "Graph.coreProgramEntry",
    externalRecordUpdateCase
      "CoreProgram module index opacity"
      ["{-# LANGUAGE DataKinds #-}"]
      [ "import Jazz.Compiler.AST (CorePhase (Resolved))",
        "import qualified Jazz.Compiler.ModuleGraph as Graph"
      ]
      "Graph.CoreProgram 'Resolved"
      "Graph.coreProgramModules"
      "Graph.coreProgramModules",
    externalRecordUpdateCase
      "ModuleExportInventory opacity"
      []
      ["import qualified Jazz.Compiler.ModuleExports as Exports"]
      "Exports.ModuleExportInventory"
      "Exports.inventoryEntries"
      "Exports.exportInventoryEntries",
    externalRecordUpdateCase
      "ModuleIdentity path opacity"
      []
      ["import qualified Jazz.Compiler.ModuleIdentity as Identity"]
      "Identity.ModuleIdentity"
      "Identity.moduleIdentityPath"
      "Identity.moduleIdentityPath",
    externalRecordUpdateCase
      "ModuleIdentity source opacity"
      []
      ["import qualified Jazz.Compiler.ModuleIdentity as Identity"]
      "Identity.ModuleIdentity"
      "Identity.moduleIdentitySource"
      "Identity.moduleIdentitySource"
  ]

externalRecordUpdateCase :: Text -> [Text] -> [Text] -> Text -> Text -> Text -> RecordUpdateCase
externalRecordUpdateCase label languagePragmas imports carrierType fieldName accessorName =
  RecordUpdateCase
    { recordUpdateLabel = label,
      recordUpdateField = Text.takeWhileEnd (/= '.') fieldName,
      recordUpdateSource =
        Text.unlines
          ( languagePragmas
              <> ["module ExternalRecordUpdate where"]
              <> imports
              <> [ "escape :: " <> carrierType <> " -> " <> carrierType,
                   "escape value = value { " <> fieldName <> " = " <> accessorName <> " value }"
                 ]
          )
    }
