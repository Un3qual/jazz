{-# LANGUAGE QuasiQuotes #-}
module ParserSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Hspec
import Data.String.QQ

import Lib
import AST


-- TODO: handle order of operations and left vs right assoc for infix expressions

simpleExprSpecs :: Spec
simpleExprSpecs = describe "Simple tests of single expressions" $ do
  context "when given a literal" $ do
    it "Should turn an Int into an ELiteral" $
      getAstFromParser "5" `shouldBe` [ ELiteral (LInt 5) ]
    it "Should turn a Float into an ELiteral" $
      getAstFromParser "5.1" `shouldBe` [ ELiteral (LFloat 5.1) ]
    it "Should turn True into an ELiteral LBool" $
      getAstFromParser "True" `shouldBe` [ ELiteral (LBool True) ]
    it "Should turn False into an ELiteral LBool" $
      getAstFromParser "False" `shouldBe` [ ELiteral (LBool False) ]
    it "Should turn a String into an ELiteral LString" $
      getAstFromParser "\"Hello, world\"" `shouldBe` [ ELiteral (LString "Hello, world") ]

  context "when given an infix expression" $ do
    it "Should turn 1 + 2 into an EInfixExpr" $
      getAstFromParser "1 + 2" `shouldBe` [EApply 
                                            (EApply
                                              (EVar (Variable {varName = "+", varType = Nothing}))
                                              (ELiteral (LInt 1)))
                                            (ELiteral (LInt 2))
                                          ]
    it "Should handle multiple infix expressions in a row" $
      getAstFromParser "1 + 2 + 3" `shouldBe` [EApply
                                                (EApply
                                                  (EVar (Variable {varName = "+", varType = Nothing}))
                                                  (EApply
                                                    (EApply
                                                      (EVar (Variable {varName = "+", varType = Nothing}))
                                                      (ELiteral (LInt 1)))
                                                    (ELiteral (LInt 2))))
                                                (ELiteral (LInt 3))
                                              ]
    it "Should handle operator precedence" $
      getAstFromParser "1 + 2 * 3" `shouldBe` [EApply
                                                (EApply
                                                  (EVar (Variable {varName = "+", varType = Nothing}))
                                                  (ELiteral (LInt 1)))
                                                (EApply
                                                  (EApply
                                                    (EVar (Variable {varName = "*", varType = Nothing}))
                                                    (ELiteral (LInt 2)))
                                                  (ELiteral (LInt 3)))
                                              ]

    it "Should handle variables as the first operand in infix expressions" $
      getAstFromParser "x + 2" `shouldBe` [EApply
                                            (EApply
                                              (EVar (Variable {varName = "+", varType = Nothing}))
                                              (EVar (Variable {varName = "x", varType = Nothing})))
                                            (ELiteral (LInt 2))]

    it "Should handle variables as the second operand in infix expressions" $
      getAstFromParser "2 + x" `shouldBe` [EApply 
                                            (EApply
                                              (EVar (Variable {varName = "+", varType = Nothing}))
                                              (ELiteral (LInt 2)))
                                            (EVar (Variable {varName = "x", varType = Nothing}))]

    it "Should handle all variables in infix expressions" $
      getAstFromParser "x + y" `shouldBe` [EApply
                                            (EApply
                                              (EVar (Variable {varName = "+", varType = Nothing}))
                                              (EVar (Variable {varName = "x", varType = Nothing})))
                                            (EVar (Variable {varName = "y", varType = Nothing}))]

  context "when given an infix operator as a prefix function call" $ do
    it "Should handle a prefix function call" $
      getAstFromParser "(+) 1 2" `shouldBe` [EApply
                                              (EApply
                                                (EVar (Variable {varName = "+", varType = Nothing}))
                                                (ELiteral (LInt 1)))
                                              (ELiteral (LInt 2))
                                            ]

    it "Should handle a left partial prefix function call" $
      getAstFromParser "(2+) 1" `shouldBe` [EApply
                                              (EApply
                                                (EVar (Variable {varName = "+", varType = Nothing}))
                                                (ELiteral (LInt 2)))
                                              (ELiteral (LInt 1))
                                            ]

    it "Should handle a right partial prefix function call" $
      getAstFromParser "(*2) 1" `shouldBe` [EApply
                                              (ELambda
                                                [FPSimple (Variable {varName = "__partialInfixLambdaParam0", varType = Nothing})]
                                                (EApply
                                                  (EApply
                                                    (EVar (Variable {varName = "*", varType = Nothing}))
                                                    (EVar (Variable {varName = "__partialInfixLambdaParam0", varType = Nothing})))
                                                  (ELiteral (LInt 2))))
                                              (ELiteral (LInt 1))
                                            ]

    -- For now, + and - in right partial infix operators are unsupported due to ambiguity with specifying a positive or negative number
    it "Should handle a right partial prefix function call with a literal operator" $
      getAstFromParser "(+2) 1" `shouldBe` [EApply
                                              (ELambda
                                                [FPSimple (Variable {varName = "__partialInfixLambdaParam0", varType = Nothing})]
                                                (EApply
                                                  (EApply
                                                    (EVar (Variable {varName = "+", varType = Nothing}))
                                                    (EVar (Variable {varName = "__partialInfixLambdaParam0", varType = Nothing})))
                                                  (ELiteral (LInt 2))))
                                              (ELiteral (LInt 1))
                                            ]
  
  context "when given a variable declaration" $ do
    it "Should turn x: Int = -5 into an ELet" $
      getAstFromParser "x: Int = -5" `shouldBe` [ELet (Variable {varName = "x", varType = Just TInt}) (EApply (EVar (Variable {varName = "-", varType = Nothing})) (ELiteral (LInt 5)))]

  context "when given a variable declaration without type information" $ do
    it "Should turn x = 5 into an ELet with the type of Nothing" $
      getAstFromParser "x = 5" `shouldBe` [ ELet (Variable "x" Nothing) (ELiteral (LInt 5)) ]

  context "when given invalid variable names" $ do
    it "Should fail to parse a variable name that starts with a number" $
      getAstFromParser "5x = 5" `shouldBe` []
    
    it "Should fail to parse a variable name that contains a special character" $
      getAstFromParser "x! = 5" `shouldBe` []
    
    -- TODO: fix this
    -- it "Should fail to parse a variable name that contains a space" $
    --   getAstFromParser "x y = 5" `shouldBe` []
    
    -- it "Should fail to parse a variable name that starts with a capital letter" $
    --   getAstFromParser "X = 5" `shouldBe` []

multipleExprSpecs :: Spec
multipleExprSpecs = describe "Simple tests of multiple expressions" $ do
  context "when given multiple expressions" $ do
    it "Should turn 1 + 2; 3 + 4 into two EApplys" $
      getAstFromParser "1 + 2; 3 + 4" `shouldBe` [ EApply (EApply (EVar (Variable "+" Nothing)) (ELiteral (LInt 1))) (ELiteral (LInt 2)), EApply (EApply (EVar (Variable "+" Nothing)) (ELiteral (LInt 3))) (ELiteral (LInt 4))]

  context "when given an assignment and an expression" $ do
    it "Should turn x: Int = 5; x + 1 into an ELet and an EApply" $
      getAstFromParser "x: Int = 5; x + 1" `shouldBe` [ ELet (Variable "x" (Just TInt)) (ELiteral (LInt 5)), EApply (EApply (EVar (Variable "+" Nothing)) (EVar (Variable "x" Nothing))) (ELiteral (LInt 1))]

simpleLambdaSpecs :: Spec
simpleLambdaSpecs = describe "Simple tests of lambda expressions" $ do
  context "when given a lambda expression" $ do
    it "Should turn \\(i: Int): Int -> i + i into an ELambda" $
      getAstFromParser "\\(i: Int): Int -> i + i" `shouldBe` [ELambda
                                                                [FPSimple (Variable "i" (Just TInt))]
                                                                (EApply
                                                                  (EApply
                                                                    (EVar (Variable "+" Nothing))
                                                                    (EVar (Variable "i" Nothing)))
                                                                  (EVar (Variable "i" Nothing)))
                                                             ]

    
    it "Should handle assignment of a lambda to a variable (function definition)" $
      getAstFromParser "f = \\(i: Int): Int -> i + i" `shouldBe` [ELet
                                                                    (Variable "f" Nothing)
                                                                    (ELambda
                                                                      [FPSimple (Variable "i" (Just TInt))]
                                                                      (EApply
                                                                        (EApply
                                                                          (EVar (Variable "+" Nothing))
                                                                          (EVar (Variable "i" Nothing)))
                                                                        (EVar (Variable "i" Nothing))))
                                                                  ]

simpleFunctionCallSpecs :: Spec
simpleFunctionCallSpecs = describe "Simple tests of function calls" $ do
  context "when given a non-parenthized function call" $ do
    it "Should turn f(5) into an EApply" $
      getAstFromParser "f(5)" `shouldBe` [EApply (EVar (Variable "f" Nothing)) (ELiteral (LInt 5))]

    it "Should turn f 5 into an EApply" $
      getAstFromParser "f 5" `shouldBe` [EApply (EVar (Variable "f" Nothing)) (ELiteral (LInt 5))]

    it "Should handle function calls with multiple aruments" $
      getAstFromParser "f 5 6" `shouldBe` [EApply (EApply (EVar (Variable "f" Nothing)) (ELiteral (LInt 5))) (ELiteral (LInt 6))]

  context "when given a $ function application" $ do
    it "Should turn f 5 $ g 6" $
      getAstFromParser "f 5 $ g 6" `shouldBe` [EApply
                                                (EVar (Variable {varName = "f", varType = Nothing}))
                                                (EApply
                                                  (ELiteral (LInt 5))
                                                  (EApply (EVar (Variable {varName = "g", varType = Nothing})) (ELiteral (LInt 6))))
                                              ]

simpleProgramSpecs :: Spec
simpleProgramSpecs = describe "Tests of simple programs" $ do
  context "when defining and calling a function" $ do
    it "Should turn double = \\(i: Int): Int -> i + i; add 5" $
      getAstFromParser [s|double = \(i: Int): Int -> i + i
double 5|] `shouldBe` [
                      ELet
                        (Variable {varName = "double", varType = Nothing})
                        (ELambda
                          [FPSimple (Variable {varName = "i", varType = Just TInt})]
                          (EApply
                            (EApply
                              (EVar (Variable {varName = "+", varType = Nothing}))
                              (EVar (Variable {varName = "i", varType = Nothing})))
                            (EVar (Variable {varName = "i", varType = Nothing})))
                        ),
                      EApply
                        (EVar (Variable {varName = "double", varType = Nothing}))
                        (ELiteral (LInt 5))
                    ]
                    -- [
                    --   ELet
                    --     (Variable {varName = "double", varType = Nothing})
                    --     (ELambda
                    --       [FPSimple (Variable {varName = "i", varType = Just TInt})]
                    --       (EApply
                    --         (EApply
                    --           (EVar (Variable {varName = "+", varType = Nothing}))
                    --           (EVar (Variable {varName = "i", varType = Nothing})))
                    --         (EApply
                    --           (EVar (Variable {varName = "i", varType = Nothing}))
                    --           (EApply (EVar (Variable {varName = "double", varType = Nothing})) (ELiteral (LInt 5))))))
                    -- ]
  