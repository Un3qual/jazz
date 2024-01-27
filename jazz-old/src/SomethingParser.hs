{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module SomethingParser where

-- import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import Data.Text (Text)
-- import Data.Void


-- type Parser = Parsec Void Text


-- newline :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
-- newline = single '\n'

-- mySequence :: Parser (Char, Char, Char)
-- mySequence = do
--   a <- char 'a'
--   b <- char 'b'
--   c <- char 'c'
--   return (a, b, c)


-- import MegaParsec.ParserT

-- type Parsec e s a = ParsecT e s Identity a
-- type Parser = Parsec Void Text

-- import Data.Text as T

-- import Text.Parsec
-- import Text.Parsec.String

-- import qualified Data.Map as Map

-- import Control.Monad.Except
-- import Control.Monad.Reader

-- data LispVal
--   = Atom T.Text
--   | List [LispVal]
--   | Number Integer
--   | String T.Text
--   | Fun IFunc
--   | Lambda IFunc EnvCtx
--   | Nil
--   | Bool Bool deriving (Eq)

-- data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

-- type EnvCtx = Map.Map T.Text LispVal

-- newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
--   deriving ( Monad
--            , Functor
--            , Applicative
--            , MonadReader EnvCtx
--            , MonadIO)




-- data Expr
--   = Num Integer
--   | BinOp Op Expr Expr
-- deriving Show

-- data Op
--   = Add
--   | Sub
--   | Mul
-- deriving Show

-- eval :: String -> Integer
-- eval = eval' . parseExpr

-- eval' :: Expr -> Integer
-- eval' (Num n) = n
-- eval' (Op Add lhs rhs) = eval' lhs + eval' rhs
-- eval' (Op Sub lhs rhs) = eval' lhs - eval' rhs
-- eval' (Op Mul lhs rhs) = eval' lhs * eval' rhs

-- parseExpr :: String -> Expr
-- parseExpr s = 
