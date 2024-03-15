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
import           Data.Functor (($>), void)
import           Data.Void
import           Data.Text (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug

type Parser = Parsec Void Text

scn :: Parser ()
scn = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")

sc :: Parser ()
sc = L.space hspace (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")
-- sc = L.space (void $ some (char ' ' <|> char '\t')) (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")
-- sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")

symbolP :: Text -> Parser Text
symbolP = L.symbol scn

lexemeP :: Parser a -> Parser a
lexemeP = L.lexeme scn

openParenP :: Parser Text
openParenP = maybeDbg "parensP::(" (symbolP "(")

closeParenP :: Parser Text
closeParenP = maybeDbg "parensP::)" (symbolP ")")

parensP :: Parser a -> Parser a
parensP = between openParenP closeParenP

openBracketP :: Parser Text
openBracketP = maybeDbg "bracketsP::[" (symbolP "[")

closeBracketP :: Parser Text
closeBracketP = maybeDbg "bracketsP::]" (symbolP "]")

bracketsP :: Parser a -> Parser a
bracketsP = between openBracketP closeBracketP

integerP :: Parser Integer
integerP = lexemeP L.decimal

callDebug :: Bool
callDebug = False

maybeDbg :: Show a => String -> Parser a -> Parser a
maybeDbg = maybeDbg' callDebug
-- maybeDbg' :: (MonadParsecDbg e s m, Show a) => Bool -> String -> m a -> m a
maybeDbg' :: (MonadParsecDbg e s m, Show a) => Bool -> String -> m a -> m a
maybeDbg' True label p = dbg label p
maybeDbg' False _ p = p