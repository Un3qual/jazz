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
    it "Should turn a list literal into an LList" $
      getAstFromParser "[1, 2, 3]" `shouldBe` [ ELiteral (LList [ELiteral (LInt 1), ELiteral (LInt 2), ELiteral (LInt 3)]) ]
    it "Should turn a list literal into an LTuple" $
      getAstFromParser "(1, 2)" `shouldBe` [ ELiteral (LTuple [ELiteral (LInt 1), ELiteral (LInt 2)]) ]
    it "Should turn a single parenthized Expr into an Expr, not an LTuple" $
      getAstFromParser "(1)" `shouldBe` [ ELiteral (LInt 1) ]

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
                                                (Just $ FPSimple (Variable {varName = "__partialInfixLambdaParam0", varType = Nothing}))
                                                (EApply
                                                  (EApply
                                                    (EVar (Variable {varName = "*", varType = Nothing}))
                                                    (EVar (Variable {varName = "__partialInfixLambdaParam0", varType = Nothing})))
                                                  (ELiteral (LInt 2))) Nothing)
                                              (ELiteral (LInt 1))
                                            ]

    -- For now, + and - in right partial infix operators are unsupported due to ambiguity with specifying a positive or negative number
    it "Should handle a right partial prefix function call with a literal operator" $
      getAstFromParser "(+2) 1" `shouldBe` [EApply
                                              (ELambda
                                                (Just $ FPSimple (Variable {varName = "__partialInfixLambdaParam0", varType = Nothing}))
                                                (EApply
                                                  (EApply
                                                    (EVar (Variable {varName = "+", varType = Nothing}))
                                                    (EVar (Variable {varName = "__partialInfixLambdaParam0", varType = Nothing})))
                                                  (ELiteral (LInt 2)))
                                                Nothing)
                                              (ELiteral (LInt 1))
                                            ]
  
  context "when given a variable declaration" $ do
    it "Should turn x: Int = -5 into an ELet" $
      getAstFromParser "x: Int = -5" `shouldBe` [ELet (Variable {varName = "x", varType = Just TInt}) (EApply (EVar (Variable {varName = "-", varType = Nothing})) (ELiteral (LInt 5)))]

    it "Should handle a variable declaration with a tuple type" $
      getAstFromParser "x: (Int, Int) = (1, 2)" `shouldBe` [ELet
                                                             (Variable {varName = "x", varType = Just (TTuple [TInt, TInt])})
                                                             (ELiteral (LTuple [ELiteral (LInt 1), ELiteral (LInt 2)]))]

    it "Should handle a variable declaration with a list type" $
      getAstFromParser "x: [Int] = [1, 2]" `shouldBe` [ELet
                                                        (Variable {varName = "x", varType = Just (TList TInt)})
                                                        (ELiteral (LList [ELiteral (LInt 1), ELiteral (LInt 2)]))]
                                                        
  context "when given a variable declaration without type information" $ do
    it "Should turn x = 5 into an ELet with the type of Nothing" $
      getAstFromParser "x = 5" `shouldBe` [ ELet (Variable "x" Nothing) (ELiteral (LInt 5)) ]

  context "when given invalid variable names" $ do
    it "Should fail to parse a variable name that starts with a number" $
      getAstFromParser "5x = 5" `shouldBe` []
    
    it "Should fail to parse a variable name that contains a special character" $
      getAstFromParser "x! = 5" `shouldBe` []
    
    -- TODO: fix this
    it "Should fail to parse a variable name that contains a space" $
      getAstFromParser "x y = 5" `shouldBe` []
    
    -- it "Should fail to parse a variable name that starts with a capital letter" $
    --   getAstFromParser "X = 5" `shouldBe` []

  context "when given a single expression across multiple lines" $ do
    it "Should turn `(add 1\\n 2)` into an EApply" $
      getAstFromParser "(add 1\n 2)" `shouldBe` [EApply (EApply (EVar (Variable "add" Nothing)) (ELiteral (LInt 1))) (ELiteral (LInt 2))]

multipleExprSpecs :: Spec
multipleExprSpecs = describe "Simple tests of multiple expressions" $ do
  context "when given multiple expressions" $ do
    it "Should turn 1 + 2. 3 + 4 into two EApplys" $
      getAstFromParser "1 + 2. 3 + 4" `shouldBe` [ EApply (EApply (EVar (Variable "+" Nothing)) (ELiteral (LInt 1))) (ELiteral (LInt 2)), EApply (EApply (EVar (Variable "+" Nothing)) (ELiteral (LInt 3))) (ELiteral (LInt 4))]

  context "when given an assignment and an expression" $ do
    it "Should turn x: Int = 5.\\n x + 1 into an ELet and an EApply" $
      getAstFromParser "x: Int = 5.\n x + 1" `shouldBe` [ ELet (Variable "x" (Just TInt)) (ELiteral (LInt 5)), EApply (EApply (EVar (Variable "+" Nothing)) (EVar (Variable "x" Nothing))) (ELiteral (LInt 1))]

simpleLambdaSpecs :: Spec
simpleLambdaSpecs = describe "Simple tests of lambda expressions" $ do
  context "when given a lambda expression" $ do
    it "Should turn \\(i: Int): Int -> i + i into an ELambda" $
      getAstFromParser "\\(i: Int): Int -> i + i" `shouldBe` [ELambda
                                                                (Just $ FPSimple (Variable "i" (Just TInt)))
                                                                (EApply
                                                                  (EApply
                                                                    (EVar (Variable "+" Nothing))
                                                                    (EVar (Variable "i" Nothing)))
                                                                  (EVar (Variable "i" Nothing)))
                                                                Nothing
                                                             ]

    
    it "Should handle assignment of a lambda to a variable (function definition)" $
      getAstFromParser "f = \\(i: Int): Int -> i + i" `shouldBe` [ELet
                                                                    (Variable "f" Nothing)
                                                                    (ELambda
                                                                      (Just $ FPSimple (Variable "i" (Just TInt)))
                                                                      (EApply
                                                                        (EApply
                                                                          (EVar (Variable "+" Nothing))
                                                                          (EVar (Variable "i" Nothing)))
                                                                        (EVar (Variable "i" Nothing)))
                                                                      Nothing)
                                                                  ]
    it "Should handle a lambda with complex type declaration" $
      getAstFromParser "f = \\((x: Int, y: Int), z: Int): Int -> x"
        `shouldBe` [ELet
                      (Variable "f" Nothing)
                      (ELambda
                        (Just $ FPPattern
                          (PatternTuple [FPSimple (Variable "x" (Just TInt)), 
                                          FPSimple (Variable "y" (Just TInt))])
                        )
                        (ELambda (Just $ FPSimple (Variable "z" (Just TInt))) (EVar (Variable "x" Nothing)) Nothing)
                        Nothing)
                    ]

  context "when given multiple types for the same parameter" $ do
    it "Should fail to parse" $
      getAstFromParser "\\((x: Int, y: Int): (Int, Int)): Int -> 5" `shouldBe` []

  context "when given a lambda with pattern params" $ do
    it "Should match a literal as a lambda parameter" $ 
      getAstFromParser "f = \\(1): Int -> 2"
        `shouldBe` [ELet
                      (Variable "f" Nothing)
                      (ELambda
                        (Just $ FPPattern (PatternLiteral (LInt 1)))
                        (ELiteral (LInt 2))
                        Nothing)
                    ]
    it "Should match an empty list as a lambda parameter" $ 
      getAstFromParser "f = \\([]): Int -> 2"
        `shouldBe` [ELet
                      (Variable "f" Nothing)
                      (ELambda
                        (Just $ FPPattern (PatternList []))
                        (ELiteral (LInt 2))
                        Nothing)
                    ]
    
    it "Should match a tuple as a lambda parameter" $
      getAstFromParser "f = \\((1, x: Int)): Int -> x"
        `shouldBe` [ELet
                      (Variable "f" Nothing)
                      (ELambda
                        (Just $ FPPattern (PatternTuple [FPPattern (PatternLiteral (LInt 1)), FPSimple (Variable "x" (Just TInt))]))
                        (EVar (Variable "x" Nothing))
                        Nothing)
                    ]
    it "Should match a list as a lambda parameter" $
      getAstFromParser "head = \\([hd | tl]): Int -> hd"
        `shouldBe` [ELet
                      (Variable "head" Nothing)
                      (ELambda
                        (Just $ FPPattern (PatternList [FPSimple (Variable "hd" Nothing),
                                                 FPSimple (Variable "tl" Nothing)]))
                        (EVar (Variable "hd" Nothing))
                        Nothing)
                    ]
                    
    it "Should match complex lambda parameters" $
      getAstFromParser "f = \\((1, [hd | tl], x: Int)): Int -> x"
        `shouldBe` [ELet
                      (Variable "f" Nothing)
                      (ELambda
                        (Just $ FPPattern (PatternTuple [FPPattern (PatternLiteral (LInt 1)), 
                                                  FPPattern (PatternList [FPSimple (Variable "hd" Nothing), 
                                                    FPSimple (Variable "tl" Nothing)]), 
                                                  FPSimple (Variable "x" (Just TInt))]))
                        (EVar (Variable "x" Nothing))
                        Nothing)
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

    it "Should handle function calls with multiple variable arguments" $
      getAstFromParser "f a b" `shouldBe` [EApply (EApply (EVar (Variable "f" Nothing)) (EVar (Variable "a" Nothing))) (EVar (Variable "b" Nothing))]

  context "when given a $ function application" $ do
    it "Should turn f 5 $ g 6" $
      getAstFromParser "f 5 $ g 6" `shouldBe` [EApply
                                                (EApply
                                                  (EVar (Variable {varName = "f", varType = Nothing}))
                                                  (ELiteral (LInt 5)))
                                                (EApply
                                                  (EVar (Variable {varName = "g", varType = Nothing}))
                                                  (ELiteral (LInt 6)))
                                              ]

simpleProgramSpecs :: Spec
simpleProgramSpecs = describe "Tests of simple programs" $ do
  context "when defining and calling a function" $ do
    it "Should turn two declarations into two expressions" $
     getAstFromParser [s|x = 1.
y = 2|] `shouldBe` [
                      ELet
                        (Variable {varName = "x", varType = Nothing})
                        (ELiteral (LInt 1)),
                      ELet
                        (Variable {varName = "y", varType = Nothing})
                        (ELiteral (LInt 2))
                    ]
    it "Should turn a function definition and call into two expressions" $
      getAstFromParser [s|
double = \(i: Int): Int ->
  i + i.
main = \() -> double 5.|] `shouldBe` [
                      ELet
                        (Variable {varName = "double", varType = Nothing})
                        (ELambda
                          (Just $ FPSimple (Variable {varName = "i", varType = Just TInt}))
                          (EApply
                            (EApply
                              (EVar (Variable {varName = "+", varType = Nothing}))
                              (EVar (Variable {varName = "i", varType = Nothing})))
                            (EVar (Variable {varName = "i", varType = Nothing})))
                          Nothing
                        ),
                      ELet
                        (Variable {varName = "main", varType = Nothing})
                        (ELambda
                          Nothing
                          (EApply
                            (EVar (Variable {varName = "double", varType = Nothing}))
                            (ELiteral (LInt 5)))
                          Nothing
                        )
                    ]

-- class Eq a {
--   (==) = \(_: a, _: a): Bool -> _
-- }
class Foldable t {
  foldl = \(fun: (a -> b -> b), init: b, foldable: t a): b
  foldl = \(fun: \(_: a, _: b): a, init: b, foldable: t a): b
}



-- TODO:
-- parse types for lambdas in decls (e.g add = \(a: Int, b: Int): Int -> a + b) results in add being of the type (Int -> Int -> Int)
-- multi-head functions in interpreter
-- polymorphic types
-- make prelude with folds
-- modules?
-- imports?
-- typeclasses
-- ADTs
-- need magic hashes that result in directly calling haskell/native code
-- intentSpecs :: Spec
-- intentSpecs = describe "Tests of indentation" $ do
--   context "when indenting the body of a top level lambda" $ do
--     it "Should accept the entire body of a TL lambda on a new line as long as it's indented"