module Jazz.Repository.HaskellSourcePolicy
  ( HaskellSourcePolicyViolation (..),
    validateHaskellSourcePolicy,
    readCompilerHaskellPolicyViolations,
  )
where

import Data.Char (isAlphaNum)
import Data.List (isPrefixOf, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (makeRelative, takeExtension, (</>))

data HaskellSourcePolicyViolation
  = PartialErrorIdentifier FilePath Int
  | PartialQualifiedMapLookup FilePath Int
  deriving (Eq, Show)

data LexicalState
  = InCode
  | InLineComment
  | InBlockComment Int
  | InStringLiteral Bool
  | InCharacterLiteral Bool

validateHaskellSourcePolicy :: FilePath -> Text -> [HaskellSourcePolicyViolation]
validateHaskellSourcePolicy path = scan InCode 1 Nothing . Text.unpack
  where
    scan _ _ _ [] = []
    scan lexicalState lineNumber previous source =
      case lexicalState of
        InCode -> scanCode lineNumber previous source
        InLineComment -> scanLineComment lineNumber source
        InBlockComment depth -> scanBlockComment depth lineNumber source
        InStringLiteral escaped -> scanStringLiteral escaped lineNumber source
        InCharacterLiteral escaped -> scanCharacterLiteral escaped lineNumber source

    scanCode _ _ [] = []
    scanCode lineNumber previous source@(character : rest)
      | "--" `isPrefixOf` source = scan InLineComment lineNumber Nothing (drop 2 source)
      | "{-" `isPrefixOf` source = scan (InBlockComment 1) lineNumber Nothing (drop 2 source)
      | character == '"' = scan (InStringLiteral False) lineNumber Nothing rest
      | character == '\'' && beginsCharacterLiteral rest =
          scan (InCharacterLiteral False) lineNumber Nothing rest
      | "error" `isPrefixOf` source,
        identifierBoundary previous,
        identifierBoundary (characterAfter 5 source) =
          PartialErrorIdentifier path lineNumber
            : scan InCode lineNumber (Just 'r') (drop 5 source)
      | "Map.!" `isPrefixOf` source,
        identifierBoundary previous,
        operatorBoundary (characterAfter 5 source) =
          PartialQualifiedMapLookup path lineNumber
            : scan InCode lineNumber (Just '!') (drop 5 source)
      | character == '\n' = scan InCode (lineNumber + 1) Nothing rest
      | otherwise = scan InCode lineNumber (Just character) rest

    scanLineComment _ [] = []
    scanLineComment lineNumber (character : rest)
      | character == '\n' = scan InCode (lineNumber + 1) Nothing rest
      | otherwise = scan InLineComment lineNumber Nothing rest

    scanBlockComment _ _ [] = []
    scanBlockComment depth lineNumber source@(character : rest)
      | "{-" `isPrefixOf` source =
          scan (InBlockComment (depth + 1)) lineNumber Nothing (drop 2 source)
      | "-}" `isPrefixOf` source =
          scan
            (if depth == 1 then InCode else InBlockComment (depth - 1))
            lineNumber
            Nothing
            (drop 2 source)
      | character == '\n' = scan (InBlockComment depth) (lineNumber + 1) Nothing rest
      | otherwise = scan (InBlockComment depth) lineNumber Nothing rest

    scanStringLiteral _ _ [] = []
    scanStringLiteral escaped lineNumber (character : rest)
      | character == '\n' = scan (InStringLiteral False) (lineNumber + 1) Nothing rest
      | escaped = scan (InStringLiteral False) lineNumber Nothing rest
      | character == '\\' = scan (InStringLiteral True) lineNumber Nothing rest
      | character == '"' = scan InCode lineNumber Nothing rest
      | otherwise = scan (InStringLiteral False) lineNumber Nothing rest

    scanCharacterLiteral _ _ [] = []
    scanCharacterLiteral escaped lineNumber (character : rest)
      | character == '\n' = scan InCode (lineNumber + 1) Nothing rest
      | escaped = scan (InCharacterLiteral False) lineNumber Nothing rest
      | character == '\\' = scan (InCharacterLiteral True) lineNumber Nothing rest
      | character == '\'' = scan InCode lineNumber Nothing rest
      | otherwise = scan (InCharacterLiteral False) lineNumber Nothing rest

readCompilerHaskellPolicyViolations :: FilePath -> IO [HaskellSourcePolicyViolation]
readCompilerHaskellPolicyViolations packageRoot = do
  let compilerRoot = packageRoot </> "src" </> "Jazz" </> "Compiler"
  sourcePaths <- listHaskellSources compilerRoot
  fmap concat . traverse (readViolations packageRoot) $ sourcePaths

readViolations :: FilePath -> FilePath -> IO [HaskellSourcePolicyViolation]
readViolations packageRoot sourcePath =
  validateHaskellSourcePolicy (makeRelative packageRoot sourcePath) <$> TextIO.readFile sourcePath

listHaskellSources :: FilePath -> IO [FilePath]
listHaskellSources directory = do
  entries <- sort <$> listDirectory directory
  fmap concat . traverse visit $ map (directory </>) entries
  where
    visit path = do
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then listHaskellSources path
        else do
          isFile <- doesFileExist path
          pure [path | isFile && takeExtension path == ".hs"]

identifierBoundary :: Maybe Char -> Bool
identifierBoundary = maybe True (not . isIdentifierCharacter)

operatorBoundary :: Maybe Char -> Bool
operatorBoundary = maybe True (not . isOperatorCharacter)

isIdentifierCharacter :: Char -> Bool
isIdentifierCharacter character =
  isAlphaNum character || character == '_' || character == '\''

isOperatorCharacter :: Char -> Bool
isOperatorCharacter character = character `elem` (":!#$%&*+./<=>?@\\^|-~" :: String)

characterAfter :: Int -> String -> Maybe Char
characterAfter offset = safeHead . drop offset

safeHead :: [value] -> Maybe value
safeHead [] = Nothing
safeHead (value : _) = Just value

beginsCharacterLiteral :: String -> Bool
beginsCharacterLiteral [] = False
beginsCharacterLiteral ('\\' : rest) = hasUnescapedClosingQuote rest
beginsCharacterLiteral (_ : '\'' : _) = True
beginsCharacterLiteral _ = False

hasUnescapedClosingQuote :: String -> Bool
hasUnescapedClosingQuote = go False
  where
    go _ [] = False
    go _ ('\n' : _) = False
    go True (_ : rest) = go False rest
    go False ('\\' : rest) = go True rest
    go False ('\'' : _) = True
    go False (_ : rest) = go False rest
