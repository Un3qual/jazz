{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Parser where

import AST
import Parser.Lib  (Parser, symbolP, maybeDbg)
import Parser.Lang (rootExprP)

import Text.Megaparsec
import Text.Megaparsec.Char (eol, hspace)

parseProgram :: Parser Program
parseProgram = do
  prog <- maybeDbg "parseProgram::programP" programP
  maybeDbg "parseProgram::eof" eof
  return prog

programP :: Parser Program
programP = do
  maybeDbg "programP::skipEol" (skipMany eol)
  -- maybeDbg "programP::expressions" (many rootExprP)
  maybeDbg "programP::separatedExprs" (sepEndBy1 (maybeDbg "programP::rootExprP" rootExprP) (maybeDbg "programP::rootSeparator" (symbolP ".")))
-- programP = maybeDbg "programP" $ skipMany (maybeDbg "parseProgram::eol" eol) *> sepEndBy (maybeDbg "programP::rootExprP" rootExprP) (many (maybeDbg "programP::eol" eol))
