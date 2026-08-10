{-# LANGUAGE OverloadedStrings #-}

module Jazz.Repository.PackagePolicy
  ( PackagePolicyViolation (..),
    renderPackagePolicyViolation,
    validatePackagePolicy,
  )
where

import Data.Char (isAlphaNum, isSpace)
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
      case [body | (header, body) <- topLevelStanzas sourceLines, normalizedHeader header == "source-repository head"] of
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
    metadataValues = fmap snd (logicalFields sourceLines)
    legacyIdentityViolations =
      [ LegacyPackageIdentity identity
      | identity <- legacyPackageIdentities,
        any (containsIdentity identity) metadataValues
      ]
    privateLibraryHeader = "library jazz-internal"
    libraryStanzas = filter (isLibraryHeader . fst) (topLevelStanzas sourceLines)
    hasPublicLibrary =
      any isPublicLibraryStanza libraryStanzas
    privateLibraryBody =
      case [body | (header, body) <- libraryStanzas, normalizedHeader header == privateLibraryHeader] of
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
          | fmap Text.toCaseFold (fieldValue "visibility" body) == Just "private" -> []
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
  case [ value
       | (indentation, candidateName, value) <- logicalFieldEntries sourceLines,
         indentation == 0,
         candidateName == Text.toCaseFold fieldName
       ] of
    [] -> Nothing
    value : _ -> Just value

fieldValue :: Text -> [Text] -> Maybe Text
fieldValue fieldName sourceLines =
  case [value | (candidateName, value) <- logicalFields sourceLines, candidateName == Text.toCaseFold fieldName] of
    [] -> Nothing
    value : _ -> Just value

logicalFields :: [Text] -> [(Text, Text)]
logicalFields sourceLines =
  [(fieldName, value) | (_, fieldName, value) <- logicalFieldEntries sourceLines]

logicalFieldEntries :: [Text] -> [(Int, Text, Text)]
logicalFieldEntries [] = []
logicalFieldEntries (line : remaining) =
  case Text.breakOn ":" (Text.stripStart line) of
    (candidateName, remainder)
      | not (Text.null (Text.strip candidateName)),
        not (Text.null remainder),
        Text.all validFieldNameCharacter (Text.strip candidateName) ->
          let indentation = Text.length (Text.takeWhile isSpace line)
              (continuations, rest) = span (isContinuation indentation) remaining
              value =
                Text.unwords
                  ( filter
                      (not . Text.null)
                      (stripComment (Text.drop 1 remainder) : fmap stripComment continuations)
                  )
           in (indentation, Text.toCaseFold (Text.strip candidateName), value)
                : logicalFieldEntries rest
    _ -> logicalFieldEntries remaining
  where
    validFieldNameCharacter character =
      isAlphaNum character || character == '-'
    isContinuation indentation candidate =
      Text.null (Text.strip candidate)
        || Text.length (Text.takeWhile isSpace candidate) > indentation

stripComment :: Text -> Text
stripComment source
  | "--" `Text.isPrefixOf` Text.stripStart source = ""
  | otherwise = Text.strip source

normalizedHeader :: Text -> Text
normalizedHeader = Text.toCaseFold . Text.unwords . Text.words . stripComment

containsIdentity :: Text -> Text -> Bool
containsIdentity identity value =
  any hasBoundaries (Text.breakOnAll foldedIdentity (Text.toCaseFold value))
  where
    foldedIdentity = Text.toCaseFold identity
    hasBoundaries (prefix, matchAndSuffix) =
      let suffix = Text.drop (Text.length foldedIdentity) matchAndSuffix
       in maybe True (not . identifierCharacter . snd) (Text.unsnoc prefix)
            && maybe True (not . identifierCharacter . fst) (Text.uncons suffix)
    identifierCharacter character =
      isAlphaNum character || character `elem` ("_'!" :: String)

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
  case Text.words (normalizedHeader header) of
    "library" : _ -> True
    _ -> False

isPublicLibraryStanza :: (Text, [Text]) -> Bool
isPublicLibraryStanza (header, body) =
  normalizedHeader header == "library"
    || fmap Text.toCaseFold (fieldValue "visibility" body) == Just "public"
