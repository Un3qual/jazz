{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeOperators #-}

module Parser.Literal where

import AST
import Parser.Lib

import           Data.Functor (($>))
import qualified Data.Text as T

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

intLiteralP :: Parser SpannedLiteral
intLiteralP = withSpan $ LInt <$> integerP

floatLiteralP :: Parser SpannedLiteral
floatLiteralP = withSpan $ LFloat <$> L.float

stringLiteralP :: Parser SpannedLiteral
stringLiteralP = withSpan $ LString . T.pack <$> (char '"' *> manyTill stringChar (char '"'))
  where
    stringChar = (char '\\' *> (char '\"' <|> char '\\')) <|> L.charLiteral

boolLiteralP :: Parser SpannedLiteral
boolLiteralP = withSpan $ (symbolP "True" $> LBool True) <|> (symbolP "False" $> LBool False)


literalP :: Parser SpannedLiteral
literalP = stringLiteralP <|> try floatLiteralP <|> intLiteralP <|> boolLiteralP

literalExprP :: Parser SpannedExpr
literalExprP = withSpan $ ELiteral <$> (stringLiteralP
                                    <|> try floatLiteralP
                                    <|> intLiteralP
                                    <|> boolLiteralP)