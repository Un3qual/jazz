module Main where

import Test.Tasty
import Test.Tasty.Hspec

import ParserSpec
import AnalyzerSpec
import Analyzer.TypeInferenceSpec
import OptimizerSpec

main :: IO ()
main = do
  simpleExprTests <- testSpec "Simple Expression Tests" simpleExprSpecs
  partialInfixTests <- testSpec "Partial Infix Operator Tests" partialInfixSpecs
  multipleExprTests <- testSpec "Multiple Expression Tests" multipleExprSpecs
  simpleLambdaTests <- testSpec "Simple Lambda Tests" simpleLambdaSpecs
  simpleFunctionCallTests <- testSpec "Simple Function Call Tests" simpleFunctionCallSpecs
  simpleProgramTests <- testSpec "Simple Program Tests" simpleProgramSpecs
  typeTests <- testSpec "Type Tests" typeSpecs
  statementTests <- testSpec "Statement Tests" statementSpecs
  constructorTests <- testSpec "Constructor Tests" constructorSpecs

  simpleExprInferTests <- testSpec "Simple Expression Type Inference Tests" simpleExprInferSpecs

  simpleAnalyzerTests <- testSpec "Simple Analyzer Tests" simpleAnalyzerSpecs
  optimizerTests <- testSpec "Optimizer Tests" constantFoldingSpecs
  
  let parserTests = [ 
                      simpleExprTests
                    , partialInfixTests
                    , multipleExprTests
                    , simpleLambdaTests
                    , simpleFunctionCallTests
                    , simpleProgramTests
                    , typeTests
                    , statementTests
                    , constructorTests
                    ]
  let typeInferenceTests = [simpleExprInferTests]
  
  let analyzerTests = [
                        -- simpleAnalyzerTests
                      ]
  let tests = testGroup "Jazz Tests" $ concat [parserTests, typeInferenceTests, [optimizerTests]]
  defaultMain tests
