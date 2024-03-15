{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeOperators #-}

module Parser.Literal where

import AST
import Parser.Lib

import           Data.Functor (($>))
import qualified Data.Text as T

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

intLiteralP :: Parser Literal
intLiteralP = LInt <$> integerP

floatLiteralP :: Parser Literal
floatLiteralP = LFloat <$> L.float

stringLiteralP :: Parser Literal
stringLiteralP = LString . T.pack <$> (char '"' *> manyTill stringChar (char '"'))
  where
    stringChar = (char '\\' *> (char '\"' <|> char '\\')) <|> L.charLiteral

boolLiteralP :: Parser Literal
boolLiteralP = (symbolP "True" $> LBool True) <|> (symbolP "False" $> LBool False)

literalP :: Parser Literal
literalP = stringLiteralP <|> try floatLiteralP <|> intLiteralP <|> boolLiteralP

literalExprP :: Parser Expr
literalExprP = ELiteral <$> (stringLiteralP
                    <|> try floatLiteralP
                    <|> intLiteralP
                    <|> boolLiteralP)