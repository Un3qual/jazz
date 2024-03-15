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
