module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Hspec

import ParserSpec
import AnalyzerSpec

main :: IO ()
main = do
  simpleExprTests <- testSpec "Simple Expression Tests" simpleExprSpecs
  multipleExprTests <- testSpec "Multiple Expression Tests" multipleExprSpecs
  simpleLambdaTests <- testSpec "Simple Lambda Tests" simpleLambdaSpecs
  simpleFunctionCallTests <- testSpec "Simple Function Call Tests" simpleFunctionCallSpecs
  simpleProgramTests <- testSpec "Simple Program Tests" simpleProgramSpecs
  typeTests <- testSpec "Type Tests" typeSpecs

  simpleAnalyzerTests <- testSpec "Simple Analyzer Tests" simpleAnalyzerSpecs
  let parserTests = [ 
                      -- simpleExprTests
                    -- , multipleExprTests
                    -- , simpleLambdaTests
                    -- , simpleFunctionCallTests
                    -- , simpleProgramTests
                     typeTests
                    ]
  let analyzerTests = [
                      -- simpleAnalyzerTests
                      ]
  let tests = testGroup "Jazz Tests" $ concat [parserTests, analyzerTests]
  defaultMain tests
