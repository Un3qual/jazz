module Main (main) where

import System.IO
import System.Environment

import Lib

import Data.Text (Text, pack, unpack)

main :: IO ()
main = do
  programFile <- head <$> getArgs
  if ((reverse . take 3 . reverse $ programFile) == ".go")
  then error "lol no generics"
  else do
    handle <- openFile programFile ReadMode
    contents <- hGetContents handle 
    putStr $ (generateJSForJazz . pack) contents

