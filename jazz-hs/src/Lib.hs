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
import Analyzer (analyze)
import Interpreter (interpret)
import AST
import System.Posix.Internals (puts)


optimize :: Program -> Program
optimize = id

getAstFromParser :: T.Text -> Program
getAstFromParser prog = case parse parseProgram "" prog of
  Left err -> []
  Right ast -> ast

runProgram :: T.Text -> IO ()
runProgram prog = case parse parseProgram "" prog of
  Left err -> putStrLn $ errorBundlePretty err
  Right ast -> do
    pPrint ast
    (evaluatedResult, finalEnv) <- interpret $ (optimize . analyze) ast
    case evaluatedResult of
      Left err -> do
        print finalEnv
        putStrLn $ show err
      Right res -> do
        print finalEnv
        pPrint res

someFunc :: IO ()
someFunc = do
  input <- fmap (T.pack . head) getArgs
  putStrLn ("Input: " ++ show input)
  runProgram input
  -- case parse parseProgram "" input of
  --   Left err -> putStrLn $ errorBundlePretty err
  --   Right prog -> do
  --     pPrint prog
