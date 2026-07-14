{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Repository.PackagePolicy
  ( PackagePolicyViolation (..),
    renderPackagePolicyViolation,
    validatePackagePolicy
  )
where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

data PackagePolicyViolation
  = PublicLibraryStanza
  | MissingPrivateLibraryStanza
  | MissingPrivateLibraryVisibility
  deriving (Eq, Show)

validatePackagePolicy :: Text -> [PackagePolicyViolation]
validatePackagePolicy source =
  publicLibraryViolations
    <> missingPrivateLibraryViolations
    <> missingPrivateVisibilityViolations
  where
    sourceLines = Text.lines source
    privateLibraryHeader = "library jazz-next-internal"
    hasPublicLibrary =
      any (\line -> isTopLevelLine line && Text.stripEnd line == "library") sourceLines
    privateLibraryBody =
      case filter (\line -> isTopLevelLine line && Text.stripEnd line == privateLibraryHeader) sourceLines of
        [] -> Nothing
        _ : _ -> Just (stanzaBody privateLibraryHeader sourceLines)
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
      "jazz-next.cabal must not declare an unnamed public library stanza"
    MissingPrivateLibraryStanza ->
      "jazz-next.cabal must declare library jazz-next-internal"
    MissingPrivateLibraryVisibility ->
      "library jazz-next-internal must declare visibility: private"

isTopLevelLine :: Text -> Bool
isTopLevelLine line =
  case Text.uncons line of
    Nothing -> False
    Just (firstCharacter, _) ->
      not (Text.null (Text.strip line))
        && not (isSpace firstCharacter)

stanzaBody :: Text -> [Text] -> [Text]
stanzaBody header linesValue =
  case dropWhile ((/= header) . Text.stripEnd) linesValue of
    [] -> []
    _ : remaining ->
      takeWhile (\line -> Text.null (Text.strip line) || not (isTopLevelLine line)) remaining
