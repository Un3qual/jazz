{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- import Lib
import System.Environment

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data ArithExpr
  = Add ArithExpr ArithExpr
  | Sub ArithExpr ArithExpr
  | Num Integer
  deriving (Show)

sc :: Parser ()
sc = L.space space1 empty empty


singleLetterP :: Parser Char
singleLetterP = char 'h'

doubleLetterP :: Parser
-- doubleLetterP :: Parser Char
-- doubleLetterP = char 'h' <* char 'a'

-- wordP :: Parser String
-- wordP = some alphaNumChar

-- boldP :: Parser String
-- boldP = count 2 (char '*') 
--     >> some (alphaNumChar <|> spaceChar) >>= \txt ->
--     count 2 (char '*') 
--     >> (return . concat) ["<b>", txt, "</b>"]

main = do
  input <- fmap head getArgs
  parseTest singleLetterP (T.pack input)



-- pScheme :: Parser Text
-- pScheme = string "data"
--   <|> string "file"
--   <|> string "ftp"
--   <|> string "http"
--   <|> string "https"
--   <|> string "irc"
--   <|> string "mailto"