module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Hspec

import ParserSpec

main :: IO ()
main = do
  simpleExprTests <- testSpec "Simple Expression Tests" simpleExprSpecs
  multipleExprTests <- testSpec "Multiple Expression Tests" multipleExprSpecs
  simpleLambdaTests <- testSpec "Simple Lambda Tests" simpleLambdaSpecs
  simpleFunctionCallTests <- testSpec "Simple Function Call Tests" simpleFunctionCallSpecs
  simpleProgramTests <- testSpec "Simple Program Tests" simpleProgramSpecs
  let tests = testGroup "Jazz Tests" [ 
                                       simpleExprTests
                                     , multipleExprTests
                                     , simpleLambdaTests
                                     , simpleFunctionCallTests
                                     , simpleProgramTests
                                     ]
  defaultMain tests
