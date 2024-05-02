{-# LANGUAGE BangPatterns, QuasiQuotes #-}
module Lib where

import System.Environment
import           Control.Applicative hiding (many, some)
import           Data.Functor (($>))
import           Data.Void
import           Data.Text (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Show.Pretty
import Parser
import Parser.Lib (Parser)

import Analyzer (analyze)
import Optimizer (optimize)
import CodeGen.Javascript (generate)

import AST
import Data.String.QQ

getAstFromParser :: T.Text -> [TExpr]
getAstFromParser prog = case parse parseProgram "" prog of
  Left err -> []
  Right ast -> map unSpan ast

getSpannedAstFromParser :: T.Text -> [SpannedExpr]
getSpannedAstFromParser prog = case parse parseProgram "" prog of
  Left err -> []
  Right ast -> ast

getTypedAstFromParser :: T.Text -> TypedProgram
getTypedAstFromParser prog = case parse parseProgram "" prog of
  Left err -> []
  Right ast -> case analyze ast of
    Left err -> []
    Right typedAst -> typedAst

testParser :: Parser SpannedExpr -> T.Text -> Either String TExpr
testParser parser str = case parse parser "" str of
  Left err -> Left $ errorBundlePretty err
  Right ast -> Right $ unSpan ast

-- runProgram :: T.Text -> IO ()
-- runProgram prog = case parse parseProgram "" prog of
--   Left err -> putStrLn $ errorBundlePretty err
--   Right ast -> do
--     pPrint ast
--     (evaluatedResult, finalEnv) <- interpret $ (optimize . analyze) ast
--     case evaluatedResult of
--       Left err -> do
--         print finalEnv
--         print err
--       Right res -> do
--         print finalEnv
--         pPrint res


generateJSForJazz :: Text -> String
generateJSForJazz prog =
  case parse parseProgram "" prog of
    Left err -> error $ errorBundlePretty err
    Right ast -> do
      case analyze ast of
        Left err -> error $ show err
        Right typedAst -> do
          let optimizedAst = optimize typedAst
          generate optimizedAst