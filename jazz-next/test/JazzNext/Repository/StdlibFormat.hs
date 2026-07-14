{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Repository.StdlibFormat
  ( StdlibFormatViolation (..),
    renderStdlibFormatViolation,
    validateStdlibModule
  )
where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (takeFileName)

data StdlibFormatViolation
  = InvalidModuleHeader FilePath
  | MissingFinalClosingBrace FilePath
  | InvalidBodyIndentation FilePath Int
  deriving (Eq, Show)

validateStdlibModule :: FilePath -> Text -> [StdlibFormatViolation]
validateStdlibModule path source
  | takeFileName path == "Prelude.jz" = []
  | otherwise = headerViolations <> closingViolations <> indentationViolations
  where
    numberedLines = zip [1 ..] (Text.lines source)
    firstLine =
      case numberedLines of
        [] -> Nothing
        (_, line) : _ -> Just line
    nonBlankLines = filter (not . Text.all isSpace . snd) numberedLines
    finalNonBlankLine =
      case reverse nonBlankLines of
        [] -> Nothing
        line : _ -> Just line
    headerViolations =
      case firstLine of
        Just line
          | "module " `Text.isPrefixOf` line,
            "{" `Text.isSuffixOf` line -> []
        _ -> [InvalidModuleHeader path]
    closingViolations =
      case finalNonBlankLine of
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
      case finalNonBlankLine of
        Nothing -> []
        Just (closingLineNumber, _) ->
          filter (\(lineNumber, _) -> lineNumber > 1 && lineNumber < closingLineNumber) numberedLines

renderStdlibFormatViolation :: StdlibFormatViolation -> Text
renderStdlibFormatViolation violation =
  case violation of
    InvalidModuleHeader path ->
      Text.pack path <> ":1: must be an unindented module header ending in {"
    MissingFinalClosingBrace path ->
      Text.pack path <> ": final non-blank line must be }"
    InvalidBodyIndentation path lineNumber ->
      Text.pack path
        <> ":"
        <> Text.pack (show lineNumber)
        <> ": must use two-space indentation levels"
