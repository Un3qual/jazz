{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Parser where

import AST
import           Control.Applicative hiding (many, some)
import           Data.Functor (($>))
import           Data.Void
-- import           Data.Map (Map)
-- import qualified Data.Map as Map
import           Data.Text (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")

symbolP :: Text -> Parser Text
symbolP = L.symbol sc

lexemeP :: Parser a -> Parser a
lexemeP = L.lexeme sc
