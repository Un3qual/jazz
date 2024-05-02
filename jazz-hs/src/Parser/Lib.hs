{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeOperators #-}
module Parser.Lib ( Parser
                  , sc
                  , sepBy2
                  , sepEndBy2
                  , symbolP
                  , lexemeP
                  , parensP
                  , bracketsP
                  , curlyBraceP
                  , integerP
                  , maybeDbg
                  , allowedIdentiferChars
                  , reservedWords
                  , withSpan
                  , liftSpan
                  , nullSpan
                  , ns
                  ) where

import           Control.Applicative hiding (many, some)
import           Control.Monad.Trans (MonadTrans, lift)
import           Data.Functor (($>), void)
import           Data.Void
import           Data.Text (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug

import AST


type Parser = Parsec Void Text

scn :: Parser ()
scn = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "{*" "*}")

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "{*" "*}")
-- sc = L.space (void $ some (char ' ' <|> char '\t')) (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")
-- sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "(*" "*)")

sepBy2 :: MonadParsec e s m => m a -> m sep -> m [a]
sepBy2 p sep = do
  first <- p
  second <- sep >> p
  rest <- many (sep >> p)
  return $ first : second : rest

sepEndBy2 :: MonadParsec e s m => m a -> m sep -> m [a]
sepEndBy2 p sep = do
  x1 <- p
  _ <- sep
  x2 <- p
  more <- option False (True <$ sep)
  if more then do
    rest <- sepEndBy2 p sep
    return (x1 : x2 : rest)
  else
    return [x1, x2]

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

openCurlyBraceP :: Parser Text
openCurlyBraceP = maybeDbg "curlyBraceP::{" (symbolP "{")
closeCurlyBraceP :: Parser Text
closeCurlyBraceP = maybeDbg "curlyBraceP::}" (symbolP "}")
curlyBraceP :: Parser a -> Parser a
curlyBraceP = between openCurlyBraceP closeCurlyBraceP

integerP :: Parser Integer
integerP = lexemeP L.decimal

reservedWords :: [Text]
reservedWords = ["import", "module", "if", "else", "let", "data", "as", "case"]

allowedIdentiferChars :: Parser Char
allowedIdentiferChars = letterChar <|> digitChar <|> char '_' <|> char '\'' <|> char '!' <|> char '$'


-- Probs better solution: 
-- getNodeInsideRangeTriple :: MonadParsec e s m => m a -> m (SourcePos, a, SourcePos)
-- getNodeInsideRangeTriple p = liftA3 (,,) getSourcePos p getSourcePos
-- withSpanT :: (Monad (m Parser), MonadTrans m) => m Parser a -> m Parser (a, Span)
-- withSpanT parser = do
--     start <- lift getSourcePos
--     ret <- parser
--     end <- lift getSourcePos
--     return (ret, Span start end)

withSpan :: Parser a -> Parser (Spanned a)
withSpan p = do
    start <- getSourcePos
    ret <- p
    end <- getSourcePos
    return $ Ann (Span start end) ret

liftSpan :: Parser a -> Parser (a, Span)
liftSpan p = do
    start <- getSourcePos
    ret <- p
    end <- getSourcePos
    return (ret, Span start end)

-- withSpanT :: MonadParsec e s m => m a -> m (Parser (Spanned a))
-- withSpanT p = do
--     start <- lift getSourcePos
--     ret <- p
--     end <- lift getSourcePos
--     return $ Ann (Span start end) ret

callDebug :: Bool
callDebug = False

maybeDbg :: Show a => String -> Parser a -> Parser a
maybeDbg = maybeDbg' callDebug
-- maybeDbg' :: (MonadParsecDbg e s m, Show a) => Bool -> String -> m a -> m a
maybeDbg' :: (MonadParsecDbg e s m, Show a) => Bool -> String -> m a -> m a
maybeDbg' True label p = dbg label p
maybeDbg' False _ p = p