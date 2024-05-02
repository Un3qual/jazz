{-# LANGUAGE QuasiQuotes #-}
module Analyzer.ScopeAnalyzerSpec where
import Test.Hspec
import Data.String.QQ

import Lib
import AST
import Parser.Lib
import Errors
import Data.Either
import Analyzer.ScopeAnalyzer

-- No tests are currently implemented for the scope analyzer because the type inferrence analyzer should handle it on its own.
-- I'll need to add some in the future, though as the language gets more complex.
-- simpleScopeAnalyserSpecs :: Spec
-- simpleScopeAnalyserSpecs = describe "Simple tests of the scope analyzer" $ do
--   context "when given a basic literal" $ do
--     it "Should handle a literal assignment" $
--       analyzeProgramScope [Ann (ns, mkTCon)]
      -- x = 5
      
      -- infer [
      --   Ann ns
      --     (ELet (Variable "x")
      --       (Ann ns (ELiteral (Ann ns (LInt 5)))))
      -- ] `shouldBe` Right [
      --   Ann (ns, TUnit)
      --   (ELet (Variable "x")
      --   (Ann (ns, mkTCon "Integer")
      --     (ELiteral (Ann (ns, mkTCon "Integer") (LInt 5)))))
      -- ]