{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.DiagnosticCatalog
  ( DiagnosticCode,
    DiagnosticMetadata (..),
    DiagnosticSeverity (..),
    WarningCategory (..),
    allDiagnosticMetadata,
    allWarningCategories,
    diagnosticCodeText,
    lookupWarningCategory,
    warningCode,
    warningHasAnalyzerEmitter,
    warningToken,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "DiagnosticCatalog" tests

tests :: [NamedTest]
tests =
  [ ("catalog publishes the exact existing inventory", testExactPublishedInventory),
    ("catalog codes are unique and well formed", testUniqueWellFormedCodes),
    ("catalog default severities match code families", testDefaultSeverities),
    ("warning metadata round trips through stable tokens", testWarningMetadataRoundTrips),
    ("warning analyzer-emitter metadata remains stable", testWarningEmitterMetadata)
  ]

testExactPublishedInventory :: IO ()
testExactPublishedInventory =
  assertEqual
    "published diagnostic code inventory"
    expectedPublishedCodes
    (map (diagnosticCodeText . metadataCode) allDiagnosticMetadata)

testUniqueWellFormedCodes :: IO ()
testUniqueWellFormedCodes = do
  let codes = map (diagnosticCodeText . metadataCode) allDiagnosticMetadata
  assertEqual "unique code count" (length codes) (length (nub codes))
  assertEqual "all codes have E#### or W#### shape" True (all isPublishedCode codes)

testDefaultSeverities :: IO ()
testDefaultSeverities = do
  let severities = [(diagnosticCodeText (metadataCode metadata), metadataDefaultSeverity metadata) | metadata <- allDiagnosticMetadata]
  assertEqual
    "error code default severities"
    True
    (all ((== SeverityError) . snd) (filter ((== "E") . Text.take 1 . fst) severities))
  assertEqual
    "warning code default severities"
    True
    (all ((== SeverityWarning) . snd) (filter ((== "W") . Text.take 1 . fst) severities))

testWarningMetadataRoundTrips :: IO ()
testWarningMetadataRoundTrips = do
  assertEqual
    "warning categories follow catalog order"
    [SameScopeRebinding, ShadowingOuterScope, UnusedBinding, DeprecatedSyntax]
    allWarningCategories
  mapM_
    ( \category -> do
        assertEqual
          ("warning token lookup for " <> warningToken category)
          (Just category)
          (lookupWarningCategory (Text.toUpper (warningToken category)))
        assertEqual
          ("warning code metadata for " <> warningToken category)
          (Just category)
          (metadataWarningCategory =<< metadataForCode (warningCode category))
    )
    allWarningCategories

testWarningEmitterMetadata :: IO ()
testWarningEmitterMetadata = do
  assertEqual "same-scope rebinding emitter" True (warningHasAnalyzerEmitter SameScopeRebinding)
  assertEqual "outer-scope shadowing emitter" True (warningHasAnalyzerEmitter ShadowingOuterScope)
  assertEqual "unused binding emitter" True (warningHasAnalyzerEmitter UnusedBinding)
  assertEqual "deprecated syntax remains reserved" False (warningHasAnalyzerEmitter DeprecatedSyntax)

metadataForCode :: DiagnosticCode -> Maybe DiagnosticMetadata
metadataForCode code =
  case filter ((== code) . metadataCode) allDiagnosticMetadata of
    [metadata] -> Just metadata
    _ -> Nothing

expectedPublishedCodes :: [Text]
expectedPublishedCodes =
  map (formatCode "E") expectedErrorNumbers
    <> map (formatCode "W") [1 .. 4]

expectedErrorNumbers :: [Int]
expectedErrorNumbers =
  [1 .. 5]
    <> [1001 .. 1007]
    <> [1010]
    <> [2001 .. 2019]
    <> [3001 .. 3003]
    <> [3006 .. 3040]
    <> [4001 .. 4016]
    <> [5001 .. 5005]

formatCode :: Text -> Int -> Text
formatCode prefix number =
  prefix <> Text.justifyRight 4 '0' (Text.pack (show number))

isPublishedCode :: Text -> Bool
isPublishedCode code =
  Text.length code == 5
    && Text.take 1 code `elem` ["E", "W"]
    && Text.all isAsciiDigit (Text.drop 1 code)
  where
    isAsciiDigit character = character >= '0' && character <= '9'
