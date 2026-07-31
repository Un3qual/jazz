{-# LANGUAGE OverloadedStrings #-}

module Jazz.Repository.JazzSourceFormat
  ( JazzSourceFormatViolation (..),
    renderJazzSourceFormatViolation,
    validateJazzModule,
  )
where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (takeFileName)

data JazzSourceFormatViolation
  = InvalidModuleHeader FilePath
  | MissingFinalClosingBrace FilePath
  | InvalidBodyIndentation FilePath Int
  | OverlongDataDeclarationLine FilePath Int Int
  | InvalidDataContinuationIndent FilePath Int
  deriving (Eq, Show)

data DataDeclarationRegion = DataDeclarationRegion
  { dataDeclarationIndent :: Int,
    dataConstructorIndent :: Maybe Int
  }

validateJazzModule :: FilePath -> Text -> [JazzSourceFormatViolation]
validateJazzModule path source
  | takeFileName path == "Prelude.jz" = dataDeclarationViolations
  | otherwise =
      headerViolations
        <> closingViolations
        <> indentationViolations
        <> dataDeclarationViolations
  where
    numberedLines = zip [1 ..] (Text.lines source)
    finalLine =
      case reverse numberedLines of
        [] -> Nothing
        line : _ -> Just line
    nonBlankLines = filter (not . Text.all isSpace . snd) numberedLines
    finalNonBlankLine =
      case reverse nonBlankLines of
        [] -> Nothing
        line : _ -> Just line
    headerViolations =
      case moduleHeaderEndLineNumber of
        Just _ -> []
        _ -> [InvalidModuleHeader path]
    moduleHeaderEndLineNumber =
      case numberedLines of
        (1, firstLine) : rest
          | "module " `Text.isPrefixOf` firstLine,
            "{" `Text.isSuffixOf` firstLine ->
              Just 1
          | "module " `Text.isPrefixOf` firstLine,
            "(" `Text.isSuffixOf` firstLine ->
              case break ((== ") {") . Text.strip . snd) rest of
                (exportLines, (lineNumber, _) : _)
                  | not (null exportLines),
                    all validExportLine exportLines ->
                      Just lineNumber
                _ -> Nothing
        _ -> Nothing
    validExportLine (_, line) =
      not (Text.null (Text.strip line))
        && Text.takeWhile isSpace line == "  "
    closingViolations =
      case finalLine of
        Just (_, "}") -> []
        _ -> [MissingFinalClosingBrace path]
    indentationViolations =
      [ InvalidBodyIndentation path lineNumber
      | (lineNumber, line) <- bodyLines,
        let leadingWhitespace = Text.takeWhile isSpace line,
        not (Text.null (Text.strip line)),
        not
          ( Text.all (== ' ') leadingWhitespace
              && Text.length leadingWhitespace >= 2
              && even (Text.length leadingWhitespace)
          )
      ]
    bodyLines =
      case (moduleHeaderEndLineNumber, finalNonBlankLine) of
        (Just headerEndLineNumber, Just (closingLineNumber, _)) ->
          filter
            (\(lineNumber, _) -> lineNumber > headerEndLineNumber && lineNumber < closingLineNumber)
            numberedLines
        _ -> []
    dataDeclarationViolations =
      validateDataDeclarations path numberedLines

validateDataDeclarations :: FilePath -> [(Int, Text)] -> [JazzSourceFormatViolation]
validateDataDeclarations path = go Nothing
  where
    go _ [] = []
    go maybeRegion ((lineNumber, line) : rest) =
      case maybeRegion of
        Nothing
          | isDataDeclarationStart line ->
              lineLengthViolations lineNumber line
                <> go
                  ( if dataDeclarationEnds line
                      then Nothing
                      else
                        Just
                          DataDeclarationRegion
                            { dataDeclarationIndent = leadingSpaces line,
                              dataConstructorIndent =
                                if "=" `Text.isInfixOf` codeBeforeComment line
                                  then Just (leadingSpaces line + 2)
                                  else Nothing
                            }
                  )
                  rest
        Nothing -> go Nothing rest
        Just region ->
          lineLengthViolations lineNumber line
            <> indentationViolations region lineNumber line
            <> go (nextRegion region line) rest

    lineLengthViolations lineNumber line
      | columns > 100 = [OverlongDataDeclarationLine path lineNumber columns]
      | otherwise = []
      where
        columns = Text.length line

    indentationViolations region lineNumber line
      | Text.null trimmed = []
      | "#" `Text.isPrefixOf` trimmed = []
      | isConstructorLine trimmed,
        leadingSpaces line /= dataDeclarationIndent region + 2 =
          [InvalidDataContinuationIndent path lineNumber]
      | isConstructorLine trimmed = []
      | leadingSpaces line /= expectedContinuationIndent region =
          [InvalidDataContinuationIndent path lineNumber]
      | otherwise = []
      where
        trimmed = Text.strip line

    nextRegion region line
      | dataDeclarationEnds line = Nothing
      | isConstructorLine (Text.strip line) =
          Just region {dataConstructorIndent = Just (leadingSpaces line)}
      | otherwise = Just region

isDataDeclarationStart :: Text -> Bool
isDataDeclarationStart =
  Text.isPrefixOf "data " . Text.stripStart

isConstructorLine :: Text -> Bool
isConstructorLine line =
  "=" `Text.isPrefixOf` line || "|" `Text.isPrefixOf` line

expectedContinuationIndent :: DataDeclarationRegion -> Int
expectedContinuationIndent region =
  case dataConstructorIndent region of
    Just constructorIndent -> constructorIndent + 2
    Nothing -> dataDeclarationIndent region + 4

leadingSpaces :: Text -> Int
leadingSpaces = Text.length . Text.takeWhile (== ' ')

dataDeclarationEnds :: Text -> Bool
dataDeclarationEnds = Text.isInfixOf "." . codeBeforeComment

codeBeforeComment :: Text -> Text
codeBeforeComment = Text.takeWhile (/= '#')

renderJazzSourceFormatViolation :: JazzSourceFormatViolation -> Text
renderJazzSourceFormatViolation violation =
  case violation of
    InvalidModuleHeader path ->
      Text.pack path <> ":1: must be an unindented module header ending in {"
    MissingFinalClosingBrace path ->
      Text.pack path <> ": final line must be }"
    InvalidBodyIndentation path lineNumber ->
      Text.pack path
        <> ":"
        <> Text.pack (show lineNumber)
        <> ": must use two-space indentation levels"
    OverlongDataDeclarationLine path lineNumber columns ->
      Text.pack path
        <> ":"
        <> Text.pack (show lineNumber)
        <> ": data declaration line has "
        <> Text.pack (show columns)
        <> " columns; maximum is 100"
    InvalidDataContinuationIndent path lineNumber ->
      Text.pack path
        <> ":"
        <> Text.pack (show lineNumber)
        <> ": data constructor payload continuation must be indented two spaces"
