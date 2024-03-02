{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeOperators #-}
module Parser.Lib ( Parser
                  , sc
                  , symbolP
                  , lexemeP
                  , parensP
                  , integerP
                  , maybeDbg
                  ) where

import AST
import           Control.Applicative hiding (many, some)
import           Data.Functor (($>))
import           Data.Void
import           Data.Text (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")

symbolP :: Text -> Parser Text
symbolP = L.symbol sc

lexemeP :: Parser a -> Parser a
lexemeP = L.lexeme sc

parensP :: Parser a -> Parser a
parensP = between (symbolP "(") (symbolP ")")

integerP :: Parser Integer
integerP = lexemeP L.decimal

callDebug :: Bool
callDebug = True

maybeDbg :: Show a => String -> Parser a -> Parser a
maybeDbg = maybeDbg' callDebug
-- maybeDbg' :: (MonadParsecDbg e s m, Show a) => Bool -> String -> m a -> m a
maybeDbg' :: (MonadParsecDbg e s m, Show a) => Bool -> String -> m a -> m a
maybeDbg' True label p = dbg label p
maybeDbg' False _ p = p