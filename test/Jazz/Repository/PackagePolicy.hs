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
  deriving (Eq, Show)

validatePackagePolicy :: Text -> [PackagePolicyViolation]
validatePackagePolicy source =
  publicLibraryViolations
    <> missingPrivateLibraryViolations
    <> missingPrivateVisibilityViolations
  where
    sourceLines = Text.lines source
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
