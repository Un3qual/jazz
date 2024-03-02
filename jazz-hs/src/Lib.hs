module Lib where

{-# LANGUAGE OverloadedStrings #-}


import System.Environment

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
import Text.Show.Pretty
import Parser
import Parser.Lib (Parser)
import AST


-- type Parser = Parsec Void Text

-- data ArithPrefixFunc
--   --       Base    Exponent
--   = APFPow ArithExp ArithExp
--   | APFSqrt ArithExp
--   | APFUnsupported
--   deriving (Show, Eq)

-- data ArithExp
--   = AAdd   ArithExp ArithExp
--   | ASub   ArithExp ArithExp
--   | AMul   ArithExp ArithExp
--   | ADiv   ArithExp ArithExp
--   | AFact  ArithExp
--   | ASqrt  ArithExp
--   | ANum   Integer
--   | AParen ArithExp
--   | AFunc  ArithPrefixFunc
--   deriving (Show, Eq)


-- sc :: Parser ()
-- sc = L.space space1 empty empty

-- symbolP :: Text -> Parser Text
-- symbolP = L.symbol sc

-- lexemeP :: Parser a -> Parser a
-- lexemeP = L.lexeme sc

-- functionP :: Parser ArithExp
-- functionP = do
--   name <- lexemeP $ T.pack <$> some letterChar
--   args <- inParens $ sepBy exprP (symbolP ",")
--   constructedName <- case name of
--     "pow" -> pure $ (APFPow (args !! 0) (args !! 1))
--     "sqrt" -> pure $ (APFSqrt (args !! 0))
--     _ -> fail $ "Unsupported function: " ++ T.unpack name
--   pure $ AFunc constructedName

-- numP :: Parser ArithExp
-- numP = ANum <$> lexemeP L.decimal

-- inParens :: Parser a -> Parser a
-- inParens = between (symbolP "(") (symbolP ")")

-- factorP :: Parser ArithExp
-- factorP = try (AFact <$> (((inParens exprP) <|> numP) <* symbolP "@"))
--       -- <|> try (ASqrt <$> (symbolP "sqrt" *> inParens exprP))
--       <|> (AParen <$> (inParens exprP))
--       <|> try functionP
--       <|> numP

-- termP :: Parser ArithExp
-- termP = do
--   lhs <- factorP
--   rhs <- many $ flip <$> operatorP <*> factorP
--   pure $ foldl (\expr f -> f expr) lhs rhs
--   where
--     operatorP = (symbolP "*" $> AMul) <|> (symbolP "/" $> ADiv)

-- exprP :: Parser ArithExp
-- exprP = do
--   lhs <- termP
--   rhs <- many $ flip <$> operatorP <*> termP
--   pure $ foldl (\expr f -> f expr) lhs rhs
--   where
--     operatorP = (symbolP "+" $> AAdd) <|> (symbolP "-" $> ASub)

parseProgram :: Parser Program
parseProgram = do
  prog <- programP
  eof
  return prog


-- calculate :: ArithExp -> Integer
-- calculate (ANum n)       = n
-- calculate (AAdd lhs rhs) = calculate lhs + calculate rhs
-- calculate (ASub lhs rhs) = calculate lhs - calculate rhs
-- calculate (AMul lhs rhs) = calculate lhs * calculate rhs
-- calculate (ADiv lhs rhs) = calculate lhs `div` calculate rhs
-- calculate (AFact n)      = product [1..calculate n]
-- calculate (ASqrt n)      = round . sqrt . fromIntegral $ calculate n
-- calculate (AParen n)     = calculate n
-- calculate (AFunc (APFPow b e)) = calculate b ^ calculate e
-- calculate (AFunc (APFSqrt n)) = round . sqrt . fromIntegral $ calculate n
-- calculate (AFunc APFUnsupported) = error $ "Unsupported function."

-- optimize :: ArithExp -> ArithExp
-- optimize (AAdd (ANum 0) rhs) = optimize rhs
-- optimize (AAdd lhs (ANum 0)) = optimize lhs
-- optimize (AParen (AParen n)) = optimize n
-- optimize n = n

-- pipeline :: ArithExp -> Integer
-- pipeline e = calculate $ optimize e


getAstFromParser :: T.Text -> Program
getAstFromParser prog = case parse parseProgram "" prog of
  Left err -> []
  Right ast -> ast


someFunc :: IO ()
someFunc = do
  input <- fmap (T.pack . head) getArgs
  putStrLn ("Input: " ++ show input)
  case parse parseProgram "" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right prog -> do
      pPrint prog
