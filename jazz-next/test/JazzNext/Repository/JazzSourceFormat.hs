{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Repository.JazzSourceFormat
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
  deriving (Eq, Show)

validateJazzModule :: FilePath -> Text -> [JazzSourceFormatViolation]
validateJazzModule path source
  | takeFileName path == "Prelude.jz" = []
  | otherwise = headerViolations <> closingViolations <> indentationViolations
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
