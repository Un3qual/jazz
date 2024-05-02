{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Parser where

import AST
import Parser.Lib  (Parser, symbolP, maybeDbg)
import Parser.Lang (rootExprP, blockP, programP)

import Text.Megaparsec
import Text.Megaparsec.Char (eol, hspace)

parseProgram :: Parser SpannedProgram
parseProgram = do
  prog <- maybeDbg "parseProgram::programP" programP
  maybeDbg "parseProgram::eof" eof
  return prog

-- parseExpressions :: Parser [Expr]
-- parseExpressions = do
--   exprs <- sepEndBy1 (maybeDbg "programP::rootExprP" rootExprP) (maybeDbg "programP::rootSeparator" (symbolP "."))
--   eof
--   return exprs

-- programP :: Parser Program
-- programP = do
--   maybeDbg "programP::skipEol" (skipMany eol)
--   -- maybeDbg "programP::expressions" (many rootExprP)
--   maybeDbg "programP::separatedExprs" (sepEndBy1 (maybeDbg "programP::rootExprP" rootExprP) (maybeDbg "programP::rootSeparator" (symbolP ".")))
--   -- some statementP
-- -- programP = maybeDbg "programP" $ skipMany (maybeDbg "parseProgram::eol" eol) *> sepEndBy (maybeDbg "programP::rootExprP" rootExprP) (many (maybeDbg "programP::eol" eol))
