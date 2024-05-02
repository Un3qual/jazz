{-# LANGUAGE QuasiQuotes #-}
module Analyzer.TypeInferenceSpec where
import Test.Hspec
import Data.String.QQ

import Lib
import AST
import Parser.Lib
import Errors
import Data.Either (isLeft)
import Analyzer.TypeInference (infer)

simpleExprInferSpecs :: Spec
simpleExprInferSpecs = describe "Simple tests of single expressions" $ do
  context "when given a basic literal" $ do
    it "Should handle a literal assignment" $
      -- x = 5
      infer [
        Ann ns
          (ELet (Variable "x")
            (Ann ns (ELiteral (Ann ns (LInt 5)))))
      ] `shouldBe` Right [
        Ann (ns, TUnit)
        (ELet (Variable "x")
        (Ann (ns, mkTCon "Integer")
          (ELiteral (Ann (ns, mkTCon "Integer") (LInt 5)))))
      ]

  context "when given a list literal" $ do
    it "Should handle a list assignment" $
      -- x = [1, 2]
      infer [
        Ann ns
          (ELet
            (Variable "x")
            (Ann ns (ELiteral (Ann ns (LList [Ann ns (ELiteral (Ann ns (LInt 1))), Ann ns (ELiteral (Ann ns (LInt 2)))])))))
      ] `shouldBe` Right [
        Ann (ns, TUnit) (ELet
          (Variable "x")
          (Ann (ns, (TList . mkTCon) "Integer")
            (ELiteral
              (Ann (ns, (TList . mkTCon) "Integer")
                (LList [
                  Ann (ns, mkTCon "Integer") (ELiteral (Ann (ns, mkTCon "Integer") (LInt 1)))
                , Ann (ns, mkTCon "Integer") (ELiteral (Ann (ns, mkTCon "Integer") (LInt 2)))])))))
      ]

    it "Should handle an empty list" $
      -- []
      infer [
        Ann ns
          (ELet
            (Variable "x")
            (Ann ns (ELiteral (Ann ns (LList [])))))
      ] `shouldBe` Right [
        Ann (ns, TUnit)
          (ELet (Variable "x")
            (Ann (ns, TList (TVar (TV "a")))
              (ELiteral (Ann (ns, TList (TVar (TV "a"))) (LList [])))))
      ]

    it "Should fail on list of different types" $
      -- [1, True]
      infer [
        Ann ns
          (ELet
            (Variable "x")
            (Ann ns (ELiteral (Ann ns (LList [Ann ns (ELiteral (Ann ns (LInt 1))), Ann ns (ELiteral (Ann ns (LBool True)))])))))
      ]
        `shouldBe` Left (TypeError (mkTCon "Integer") (TCon (TC "Bool") []))

    it "Should fail on list of different types same trait" $
      -- [1, 1.0]
      infer [
        Ann ns
          (ELet
            (Variable "x")
            (Ann ns (ELiteral (Ann ns (LList [Ann ns (ELiteral (Ann ns (LInt 1))), Ann ns (ELiteral (Ann ns (LFloat 1.0)))])))))
      ]
        `shouldBe` Left (TypeError (mkTCon "Integer") (TCon (TC "Float") []))

  context "when given a tuple literal" $ do
    it "Should handle a tuple literal" $
      -- (1, True)
      infer [
        Ann ns
          (ELiteral (Ann ns
            (LTuple [
              Ann ns (ELiteral (Ann ns (LInt 1))), Ann ns (ELiteral (Ann ns (LBool True)))
            ])))
      ] `shouldBe` Right [
        Ann (ns, TTuple [mkTCon "Integer",TCon (TC "Bool") []]) (ELiteral
          (Ann (ns, TTuple [mkTCon "Integer", TCon (TC "Bool") []]) (LTuple [
            Ann (ns, mkTCon "Integer") (ELiteral (Ann (ns, mkTCon "Integer") (LInt 1))),
            Ann (ns, TCon (TC "Bool") []) (ELiteral (Ann (ns, TCon (TC "Bool") []) (LBool True)))
            ])))
      ]
  context "when given function application" $ do
    it "Should handle simple application with a builtin function" $
      -- 1 + 2
      infer [
        Ann ns (EApply
          (Ann ns (EApply
            (Ann ns (EVar (Variable "+")))
            (Ann ns (ELiteral (Ann ns (LInt 1)))))
          )
          (Ann ns (ELiteral (Ann ns (LInt 2)))))
      ] `shouldBe` Right [
        Ann (ns, mkTCon "Integer")
          (EApply
            (Ann (ns, TLambda (mkTCon "Integer") (mkTCon "Integer"))
              (EApply 
                (Ann (ns, TLambda (mkTCon "Integer") (TLambda (mkTCon "Integer") (mkTCon "Integer"))) (EVar (Variable "+")))
                (mkTypedLIntExpr 1)))
            (mkTypedLIntExpr 2))
      ]

    it "Should infer type of simple + operator" $
      -- add = (+)
      infer [
        Ann ns
          (ELet (Variable "add")
            (Ann ns (EVar (Variable "+"))))
      ] `shouldBe` Right [
        Ann (ns, TUnit)
          (ELet (Variable "add")
            (Ann (ns, TLambda (mkTCon "Integer") (TLambda (mkTCon "Integer") (mkTCon "Integer"))) -- Integer -> Integer -> Integer
              (EVar (Variable "+"))))
      ]

  context "when given a lambda" $ do
    it "Should handle a simple lambda" $
      -- \i -> i == i
      infer [
          Ann ns
            (ELambda
              (Just $ Ann ns $ FPSimple (Variable "i"))
              (Ann ns (EApply
                (Ann ns (EApply
                  (Ann ns (EVar (Variable "==")))
                  (Ann ns (EVar (Variable "i"))))
                )
                (Ann ns (EVar (Variable "i"))))))
        ] `shouldBe` Right [
          Ann (ns, TLambda (mkTCon "Integer") (mkTCon "Bool"))
            (ELambda
              (Just (Ann (ns, mkTCon "Integer") (FPSimple (Variable "i"))))
              (Ann (ns, mkTCon "Bool")
                (EApply
                  (Ann (ns, TLambda (mkTCon "Integer") (mkTCon "Bool"))
                    (EApply
                      (Ann (ns, TLambda (mkTCon "Integer") (TLambda (mkTCon "Integer") (mkTCon "Bool")))
                        (EVar (Variable "==")))
                      (Ann (ns, mkTCon "Integer") (EVar (Variable "i")))))
                  (Ann (ns, mkTCon "Integer") (EVar (Variable "i"))))))
        ]

  -- Currently broken
  -- context "when given a type signature" $ do
  --   -- x :: Int
  --   -- x = 5
  --   it "Should handle a simple type signature" $
  --     infer [
  --       Ann ns (ETypeSignature (Variable "x") [] (mkTCon "Integer")),
  --       Ann ns (ELet (Variable "x")
  --         (Ann ns (ELiteral (Ann ns (LInt 5)))))
  --     ] `shouldBe` Right [
  --       Ann (ns, TUnit) (ETypeSignature (Variable "x") [] (mkTCon "Integer")),
  --       Ann (ns, TUnit) (ELet (Variable "x")
  --         (Ann (ns, mkTCon "Integer") (ELiteral (Ann (ns, mkTCon "Integer") (LInt 5)))))
  --     ]

  context "when given a block" $ do
    it "Should handle a simple block" $
      -- \i -> {
      --  xDoubled = i + i
      --  x + 1  
      --  True
      -- }
      infer [
        Ann ns (ELambda (Just $ Ann ns $ FPSimple (Variable "i")) (Ann ns (EBlock [
          Ann ns (ELet (Variable "xDoubled") (
            Ann ns (EApply
              (Ann ns (EApply
                (Ann ns (EVar (Variable "+")))
                (Ann ns (EVar (Variable "i"))))
              )
              (Ann ns (EVar (Variable "i"))))
          )),
          Ann ns (EApply
            (Ann ns (EApply
              (Ann ns (EVar (Variable "+")))
              (Ann ns (EVar (Variable "i"))))
            )
            (Ann ns (ELiteral (Ann ns (LInt 1))))),
          Ann ns (ELiteral (Ann ns (LBool True)))
        ])))
      ] `shouldBe` Right [
        Ann (ns, TLambda (mkTCon "Integer") (mkTCon "Bool")) (ELambda (Just $ Ann (ns, mkTCon "Integer") $ FPSimple (Variable "i")) (Ann (ns, mkTCon "Bool") (EBlock [
          Ann (ns, TUnit) (ELet (Variable "xDoubled") (
            Ann (ns, mkTCon "Integer") (EApply
            (Ann (ns, TLambda (mkTCon "Integer") (mkTCon "Integer"))
              (EApply 
                (Ann (ns, TLambda (mkTCon "Integer") (TLambda (mkTCon "Integer") (mkTCon "Integer"))) (EVar (Variable "+")))
                (Ann (ns, mkTCon "Integer") (EVar (Variable "i")))))
            (Ann (ns, mkTCon "Integer") (EVar (Variable "i"))))
          )),
          Ann (ns, mkTCon "Integer") (EApply
            (Ann (ns, TLambda (mkTCon "Integer") (mkTCon "Integer"))
              (EApply 
                (Ann (ns, TLambda (mkTCon "Integer") (TLambda (mkTCon "Integer") (mkTCon "Integer"))) (EVar (Variable "+")))
                (Ann (ns, mkTCon "Integer") (EVar (Variable "i")))))
            (mkTypedLIntExpr 1)),
          Ann (ns, mkTCon "Bool") (ELiteral (Ann (ns, mkTCon "Bool") (LBool True)))
        ])))
      ]

  context "when given multiple expressions" $ do
    it "should maintain the environment across multiple expressions" $
      -- x = 5
      -- y = x + x
      -- y
      infer [
        Ann ns (ELet (Variable "x") (Ann ns (ELiteral (Ann ns (LInt 5))))),
        Ann ns (ELet (Variable "y") (Ann ns (EApply
          (Ann ns (EApply
            (Ann ns (EVar (Variable "+")))
            (Ann ns (EVar (Variable "x"))))
          )
          (Ann ns (EVar (Variable "x")))))),
        Ann ns (EVar (Variable "y"))
      ] `shouldBe` Right [
        Ann (ns, TUnit) (ELet (Variable "x") (Ann (ns, mkTCon "Integer") (ELiteral (Ann (ns, mkTCon "Integer") (LInt 5))))),
        Ann (ns, TUnit) (ELet (Variable "y") (Ann (ns, mkTCon "Integer") (EApply
          (Ann (ns, TLambda (mkTCon "Integer") (mkTCon "Integer"))
            (EApply 
              (Ann (ns, TLambda (mkTCon "Integer") (TLambda (mkTCon "Integer") (mkTCon "Integer"))) (EVar (Variable "+")))
              (Ann (ns, mkTCon "Integer") (EVar (Variable "x")))))
          (Ann (ns, mkTCon "Integer") (EVar (Variable "x")))))),
        Ann (ns, mkTCon "Integer") (EVar (Variable "y"))
      ]