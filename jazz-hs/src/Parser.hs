{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Parser where

import AST
import Parser.Lib  (Parser, symbolP)
import Parser.Lang (rootExprP)

import Text.Megaparsec

programP :: Parser Program
programP = sepBy rootExprP (symbolP ";")

  -- <|> multiplicativeP
  -- <|> additiveP
  -- <|> termP
  -- <|> factorP
  -- <|> numP
  -- <|> functionP
  -- <|> try (ASqrt <$> (symbolP "sqrt" *> inParens exprP))
  -- <|> (AParen <$> (inParens exprP))
  -- <|> try functionP
  -- <|> numP




-- factorP :: Parser ArithExp
-- factorP = try (AFact <$> (((inParens exprP) <|> numP) <* symbolP "@"))
--       -- <|> try (ASqrt <$> (symbolP "sqrt" *> inParens exprP))
--       <|> (AParen <$> (inParens exprP))
--       <|> try functionP
--       <|> numP

-- multiplicativeP :: Parser Expr
-- multiplicativeP = do
--   lhs <- factorP
--   rhs <- many $ flip <$> operatorP <*> factorP
--   pure $ foldl (\expr f -> f expr) lhs rhs
--   where
--     operatorP = (symbolP "*" $> AMul) <|> (symbolP "/" $> ADiv)

-- additiveP :: Parser Expr
-- additiveP = do
--   lhs <- termP
--   rhs <- many $ flip <$> operatorP <*> termP
--   pure $ foldl (\expr f -> f expr) lhs rhs
--   where
--     operatorP = (symbolP "+" $> AAdd) <|> (symbolP "-" $> ASub)
