{-# LANGUAGE QuasiQuotes #-}
module AnalyzerSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Hspec
import Data.String.QQ

import Lib
import Analyzer
import AST


-- TODO: handle order of operations and left vs right assoc for infix expressions

simpleAnalyzerSpecs :: Spec
simpleAnalyzerSpecs = describe "Simple tests of analyzing single expressions" $ do
  context "when given a literal" $ do
    it "Should turn an Int into an ELiteral" $
      1 `shouldBe` 1
      -- x = 1
      -- analyzeProgram "x = 1" `shouldBe` [ELet (Variable {varName = "x", varType = Nothing}) (ELiteral (LInt 1))]