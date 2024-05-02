{-# LANGUAGE QuasiQuotes #-}
module OptimizerSpec where
import Test.Hspec
import Data.String.QQ

import Lib
import AST
import Optimizer.ConstantFolding


-- TODO: handle order of operations and left vs right assoc for infix expressions

constantFoldingSpecs :: Spec
constantFoldingSpecs = describe "Tests of constant folding" $ do
  context "when given numeric binary operation of two literals" $ do
    it "should convert them into just their sum" $ do
      let expr = Ann (ns, mkTCon "Integer") (EApply (Ann (ns, TLambda (mkTCon "Integer") (mkTCon "Integer")) (EApply (Ann (ns, TLambda (mkTCon "Integer") (TLambda (mkTCon "Integer") (mkTCon "Integer"))) (EVar (Variable "+"))) (mkTypedLIntExpr 1))) (mkTypedLIntExpr 2))
      constantFold expr `shouldBe` Ann (ns, mkTCon "Integer") (ELiteral (Ann (ns, mkTCon "Integer") (LInt 3)))

    it "should convert them into just their product" $ do
      let expr = Ann (ns, mkTCon "Integer") (EApply (Ann (ns, TLambda (mkTCon "Integer") (mkTCon "Integer")) (EApply (Ann (ns, TLambda (mkTCon "Integer") (TLambda (mkTCon "Integer") (mkTCon "Integer"))) (EVar (Variable "*"))) (mkTypedLIntExpr 1))) (mkTypedLIntExpr 2))
      constantFold expr `shouldBe` Ann (ns, mkTCon "Integer") (ELiteral (Ann (ns, mkTCon "Integer") (LInt 2)))
      