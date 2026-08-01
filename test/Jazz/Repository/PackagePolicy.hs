{-# LANGUAGE OverloadedStrings #-}

module Jazz.Repository.PackagePolicy
  ( PackagePolicyViolation (..),
    renderPackagePolicyViolation,
    validatePackagePolicy,
  )
where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

data PackagePolicyViolation
  = PublicLibraryStanza
  | MissingPrivateLibraryStanza
  | MissingPrivateLibraryVisibility
  | InvalidPackageField Text Text (Maybe Text)
  | MissingHeadSourceRepository
  | InvalidHeadSourceRepositoryField Text Text (Maybe Text)
  | LegacyPackageIdentity Text
  deriving (Eq, Show)

validatePackagePolicy :: Text -> [PackagePolicyViolation]
validatePackagePolicy source =
  packageFieldViolations
    <> sourceRepositoryViolations
    <> legacyIdentityViolations
    <> publicLibraryViolations
    <> missingPrivateLibraryViolations
    <> missingPrivateVisibilityViolations
  where
    sourceLines = Text.lines source
    packageFieldViolations =
      [ InvalidPackageField fieldName expectedValue actualValue
      | (fieldName, expectedValue) <- requiredPackageFields,
        let actualValue = topLevelFieldValue fieldName sourceLines,
        actualValue /= Just expectedValue
      ]
    sourceRepositoryBody =
      case [body | (header, body) <- topLevelStanzas sourceLines, header == "source-repository head"] of
        [] -> Nothing
        body : _ -> Just body
    sourceRepositoryViolations =
      case sourceRepositoryBody of
        Nothing -> [MissingHeadSourceRepository]
        Just body ->
          [ InvalidHeadSourceRepositoryField fieldName expectedValue actualValue
          | (fieldName, expectedValue) <- requiredHeadSourceRepositoryFields,
            let actualValue = fieldValue fieldName body,
            actualValue /= Just expectedValue
          ]
    normalizedSource = Text.toCaseFold source
    legacyIdentityViolations =
      [ LegacyPackageIdentity identity
      | identity <- legacyPackageIdentities,
        Text.toCaseFold identity `Text.isInfixOf` normalizedSource
      ]
    privateLibraryHeader = "library jazz-internal"
    libraryStanzas = filter (isLibraryHeader . fst) (topLevelStanzas sourceLines)
    hasPublicLibrary =
      any isPublicLibraryStanza libraryStanzas
    privateLibraryBody =
      case [body | (header, body) <- libraryStanzas, header == privateLibraryHeader] of
        [] -> Nothing
        body : _ -> Just body
    publicLibraryViolations =
      [PublicLibraryStanza | hasPublicLibrary]
    missingPrivateLibraryViolations =
      case privateLibraryBody of
        Nothing -> [MissingPrivateLibraryStanza]
        Just _ -> []
    missingPrivateVisibilityViolations =
      case privateLibraryBody of
        Nothing -> []
        Just body
          | any ((== "visibility: private") . Text.strip) body -> []
          | otherwise -> [MissingPrivateLibraryVisibility]

renderPackagePolicyViolation :: PackagePolicyViolation -> Text
renderPackagePolicyViolation violation =
  case violation of
    PublicLibraryStanza ->
      "jazz.cabal must not declare public library stanzas"
    MissingPrivateLibraryStanza ->
      "jazz.cabal must declare library jazz-internal"
    MissingPrivateLibraryVisibility ->
      "library jazz-internal must declare visibility: private"
    InvalidPackageField fieldName expectedValue Nothing ->
      "jazz.cabal must declare " <> fieldName <> ": " <> expectedValue
    InvalidPackageField fieldName expectedValue (Just actualValue) ->
      "jazz.cabal must declare "
        <> fieldName
        <> ": "
        <> expectedValue
        <> " (found "
        <> actualValue
        <> ")"
    MissingHeadSourceRepository ->
      "jazz.cabal must declare source-repository head"
    InvalidHeadSourceRepositoryField fieldName expectedValue Nothing ->
      "source-repository head must declare " <> fieldName <> ": " <> expectedValue
    InvalidHeadSourceRepositoryField fieldName expectedValue (Just actualValue) ->
      "source-repository head must declare "
        <> fieldName
        <> ": "
        <> expectedValue
        <> " (found "
        <> actualValue
        <> ")"
    LegacyPackageIdentity identity ->
      "jazz.cabal must not reference legacy product identity " <> identity

requiredPackageFields :: [(Text, Text)]
requiredPackageFields =
  [ ("name", "jazz"),
    ("synopsis", "A statically typed functional language with practical syntax"),
    ("homepage", "https://un3qual.github.io/jazz/"),
    ("bug-reports", "https://github.com/un3qual/jazz/issues"),
    ("author", "un3qual"),
    ("maintainer", "un3qual"),
    ("category", "Language"),
    ("stability", "Experimental"),
    ("tested-with", "GHC == 9.14.1"),
    ("license", "GPL-3.0-only"),
    ("license-file", "LICENSE")
  ]

requiredHeadSourceRepositoryFields :: [(Text, Text)]
requiredHeadSourceRepositoryFields =
  [ ("type", "git"),
    ("location", "https://github.com/un3qual/jazz.git")
  ]

legacyPackageIdentities :: [Text]
legacyPackageIdentities =
  [ "jazz-next",
    "JazzNext",
    "jazz-hs",
    "jazz2"
  ]

topLevelFieldValue :: Text -> [Text] -> Maybe Text
topLevelFieldValue fieldName sourceLines =
  fieldValue fieldName (filter isTopLevelLine sourceLines)

fieldValue :: Text -> [Text] -> Maybe Text
fieldValue fieldName sourceLines =
  case [ Text.strip (Text.drop 1 remainder)
       | line <- sourceLines,
         let (candidateName, remainder) = Text.breakOn ":" (Text.strip line),
         candidateName == fieldName,
         not (Text.null remainder)
       ] of
    [] -> Nothing
    value : _ -> Just value

isTopLevelLine :: Text -> Bool
isTopLevelLine line =
  case Text.uncons line of
    Nothing -> False
    Just (firstCharacter, _) ->
      not (Text.null (Text.strip line))
        && not (isSpace firstCharacter)

topLevelStanzas :: [Text] -> [(Text, [Text])]
topLevelStanzas [] = []
topLevelStanzas (line : remaining)
  | not (isTopLevelLine line) = topLevelStanzas remaining
  | otherwise =
      let (body, rest) =
            span (\bodyLine -> Text.null (Text.strip bodyLine) || not (isTopLevelLine bodyLine)) remaining
       in (Text.stripEnd line, body) : topLevelStanzas rest

isLibraryHeader :: Text -> Bool
isLibraryHeader header =
  case Text.words header of
    "library" : _ -> True
    _ -> False

isPublicLibraryStanza :: (Text, [Text]) -> Bool
isPublicLibraryStanza (header, body) =
  header == "library"
    || any ((== "visibility: public") . Text.strip) body
