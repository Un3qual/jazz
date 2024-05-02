{-# LANGUAGE QuasiQuotes #-}
module ParserSpec where
import Test.Hspec
import Data.String.QQ

import Lib
import AST
import Parser.Lang as LP
import Data.Either (isLeft)


-- TODO: handle order of operations and left vs right assoc for infix expressions

simpleExprSpecs :: Spec
simpleExprSpecs = describe "Simple tests of single expressions" $ do
  context "when given a literal" $ do
    it "Should turn an Int into an ELiteral" $
      getAstFromParser "5" `shouldBe` [ TELiteral (TLInt 5) ]
    it "Should turn a Float into an ELiteral" $
      getAstFromParser "5.1" `shouldBe` [ TELiteral (TLFloat 5.1) ]
    it "Should turn True into an ELiteral LBool" $
      getAstFromParser "True" `shouldBe` [ TELiteral (TLBool True) ]
    it "Should turn False into an ELiteral LBool" $
      getAstFromParser "False" `shouldBe` [ TELiteral (TLBool False) ]
    it "Should turn a String into an ELiteral LString" $
      getAstFromParser "\"Hello, world\"" `shouldBe` [ TELiteral (TLString "Hello, world") ]
    it "Should turn a list literal into an LList" $
      getAstFromParser "[1, 2, 3]" `shouldBe` [ TELiteral (TLList [TELiteral (TLInt 1), TELiteral (TLInt 2), TELiteral (TLInt 3)]) ]
    it "Should turn a list literal into an LTuple" $
      getAstFromParser "(1, 2)" `shouldBe` [ TELiteral (TLTuple [TELiteral (TLInt 1), TELiteral (TLInt 2)]) ]
    it "Should turn a single parenthized Expr into an Expr, not an LTuple" $
      getAstFromParser "(1)" `shouldBe` [ TELiteral (TLInt 1) ]

  context "when given internals" $ do
    it "Should handle an internal function call" $
      getAstFromParser "$add 1 2" `shouldBe` [TEApply (TEApply (TEVar (Variable "$add")) (TELiteral (TLInt 1))) (TELiteral (TLInt 2))]

    it "Should handle an internal type" $
      getAstFromParser "x :: $Int" `shouldBe` [TETypeSignature (Variable "x") [] (TCon (TC "$Int") [])]

  context "when given an infix expression" $ do
    it "Should turn 1 + 2 into an EApply" $ do
      getAstFromParser "1 + 2" `shouldBe` [TEApply
                                            (TEApply
                                              (TEVar (Variable "+"))
                                              (TELiteral (TLInt 1)))
                                            (TELiteral (TLInt 2))
                                          ]
    it "Should handle multiple infix expressions in a row" $
      getAstFromParser "1 + 2 + 3" `shouldBe` [TEApply
                                                (TEApply
                                                  (TEVar (Variable "+"))
                                                  (TEApply
                                                    (TEApply
                                                      (TEVar (Variable "+"))
                                                      (TELiteral (TLInt 1)))
                                                    (TELiteral (TLInt 2))))
                                                (TELiteral (TLInt 3))
                                              ]
    it "Should handle operator precedence" $
      getAstFromParser "1 + 2 * 3" `shouldBe` [TEApply
                                                (TEApply
                                                  (TEVar (Variable "+"))
                                                  (TELiteral (TLInt 1)))
                                                (TEApply
                                                  (TEApply
                                                    (TEVar (Variable "*"))
                                                    (TELiteral (TLInt 2)))
                                                  (TELiteral (TLInt 3)))
                                              ]

    it "Should handle variables as the first operand in infix expressions" $
      getAstFromParser "x + 2" `shouldBe` [TEApply
                                            (TEApply
                                              (TEVar (Variable "+"))
                                              (TEVar (Variable "x")))
                                            (TELiteral (TLInt 2))]

    it "Should handle variables as the second operand in infix expressions" $
      getAstFromParser "2 + x" `shouldBe` [TEApply
                                            (TEApply
                                              (TEVar (Variable "+"))
                                              (TELiteral (TLInt 2)))
                                            (TEVar (Variable "x"))]

    it "Should handle all variables in infix expressions" $
      getAstFromParser "x + y" `shouldBe` [TEApply
                                            (TEApply
                                              (TEVar (Variable "+"))
                                              (TEVar (Variable "x")))
                                            (TEVar (Variable "y"))]

  context "when given a type signature" $ do
    it "Should handle a simple type signature" $
      getAstFromParser "x :: Integer" `shouldBe` [TETypeSignature (Variable "x") [] (mkTCon "Integer")]

  context "when given a variable declaration" $ do
    it "Should handle a variable declaration without type information" $
      getAstFromParser "x = 5" `shouldBe` [TELet (Variable "x") (TELiteral (TLInt 5))]

    it "Should handle a variable declaration with a type signature" $
      getAstFromParser "x :: Integer.\nx = 5" `shouldBe`
        [
          TETypeSignature
            (Variable "x")
            []
            (mkTCon "Integer")
        , TELet
            (Variable "x")
            (TELiteral (TLInt 5))
        ]

    it "Should handle a variable declaration with a tuple type" $
      getAstFromParser "x :: (Integer, Integer).\nx = (1, 2)" `shouldBe`
        [
          TETypeSignature
            (Variable "x")
            []
            (TTuple [mkTCon "Integer", mkTCon "Integer"]),
          TELet
            (Variable "x")
            (TELiteral (TLTuple [TELiteral (TLInt 1), TELiteral (TLInt 2)]))
        ]

    it "Should handle a variable declaration with a list type" $
      getAstFromParser "x :: [Integer].\nx = [1, 2]" `shouldBe`
        [
          TETypeSignature
            (Variable "x")
            []
            (TList (mkTCon "Integer")),
          TELet
            (Variable "x")
            (TELiteral (TLList [TELiteral (TLInt 1), TELiteral (TLInt 2)]))
        ]

  context "when given a variable declaration without type information" $ do
    it "Should turn x = 5 into an TELet with the type of Nothing" $
      getAstFromParser "x = 5" `shouldBe` [ TELet (Variable "x") (TELiteral (TLInt 5)) ]

    it "Should parse _ as an identifier" $
      getAstFromParser "_ = 5" `shouldBe` [ TELet (Variable "_") (TELiteral (TLInt 5)) ]

  context "when given invalid variable names" $ do
    -- it "Should fail to parse a variable name that starts with a number" $
    --   getAstFromParser "5x = 5" `shouldBe` []

    -- TODO: fix this
    it "Should fail to parse a variable name that contains a space" $
      getAstFromParser "x y = 5" `shouldBe` []

    -- it "Should fail to parse a variable name that starts with a capital letter" $
    --   getAstFromParser "X = 5" `shouldBe` []

  context "when given a single expression across multiple lines" $ do
    it "Should turn `(add 1\\n 2)` into an TEApply" $
      getAstFromParser "(add 1\n 2)" `shouldBe` [TEApply (TEApply (TEVar (Variable "add")) (TELiteral (TLInt 1))) (TELiteral (TLInt 2))]

partialInfixSpecs :: Spec
partialInfixSpecs = describe "Tests of partial infix operators" $ do
  context "When declaring an infix operator" $ do
    it "Should handle infix operators as variable names in lets" $
        getAstFromParser "(+) = add" `shouldBe` [TELet (Variable "+") (TEVar (Variable "add"))]

  context "when given an infix operator as a prefix function call" $ do
    it "Should handle a prefix function call" $
      getAstFromParser "(+) 1 2" `shouldBe` [TEApply
                                              (TEApply
                                                (TEVar (Variable "+"))
                                                (TELiteral (TLInt 1)))
                                              (TELiteral (TLInt 2))
                                            ]

    it "Should handle a left partial prefix function call" $
      getAstFromParser "(2+) 1" `shouldBe` [TEApply
                                              (TEApply
                                                (TEVar (Variable "+"))
                                                (TELiteral (TLInt 2)))
                                              (TELiteral (TLInt 1))
                                            ]

    -- TODO: Stop using the lambda hack
    it "Should handle a right partial prefix function call" $
      getAstFromParser "(*2) 1" `shouldBe` [TEApply
                                              (TELambda
                                                (Just $ TFPSimple (Variable "__partialInfixLambdaParam0"))
                                                (TEApply
                                                  (TEApply
                                                    (TEVar (Variable "*"))
                                                    (TEVar (Variable "__partialInfixLambdaParam0")))
                                                  (TELiteral (TLInt 2))))
                                              (TELiteral (TLInt 1))
                                            ]

  --   -- For now, + and - in right partial infix operators are unsupported due to ambiguity with specifying a positive or negative number
    it "Should handle a right partial prefix function call with a literal operator" $
      getAstFromParser "(+2) 1" `shouldBe` [TEApply
                                              (TELambda
                                                (Just $ TFPSimple (Variable "__partialInfixLambdaParam0"))
                                                (TEApply
                                                  (TEApply
                                                    (TEVar (Variable "+"))
                                                    (TEVar (Variable "__partialInfixLambdaParam0")))
                                                  (TELiteral (TLInt 2))))
                                              (TELiteral (TLInt 1))
                                            ]

  context "When assigning a partial infix operator to a variable" $ do
    it "Should handle a left partial infix-as-prefix in a declaration" $
      getAstFromParser "add2Left = (2+)" `shouldBe` [TELet
                                                  (Variable "add2Left")
                                                  ((TEApply
                                                    (TEVar (Variable "+"))
                                                    (TELiteral (TLInt 2))))]
    it "Should handle a right partial infix-as-prefix in a declaration" $
      getAstFromParser "add2Right = (*2)" `shouldBe` [TELet
                                                  (Variable "add2Right")
                                                  (TELambda
                                                    (Just $ TFPSimple (Variable "__partialInfixLambdaParam0"))
                                                    (TEApply
                                                      (TEApply
                                                        (TEVar (Variable "*"))
                                                        (TEVar (Variable "__partialInfixLambdaParam0")))
                                                      (TELiteral (TLInt 2))))]

  context "When using partial infix operators in function calls" $ do
    it "Should handle a left partial infix-as-prefix in a function call" $
      getAstFromParser "map (2+) myList" `shouldBe` [TEApply
                                                      (TEApply
                                                        (TEVar (Variable "map"))
                                                        ((TEApply
                                                          (TEVar (Variable "+"))
                                                          (TELiteral (TLInt 2))))
                                                        )
                                                      (TEVar (Variable "myList"))]

    it "Should handle a right partial infix-as-prefix in a function call" $
      getAstFromParser "map (*2) myList" `shouldBe` [TEApply
                                                      (TEApply
                                                        (TEVar (Variable "map"))
                                                        ((TELambda
                                                          (Just $ TFPSimple (Variable "__partialInfixLambdaParam0"))
                                                          (TEApply
                                                            (TEApply
                                                              (TEVar (Variable "*"))
                                                              (TEVar (Variable "__partialInfixLambdaParam0")))
                                                            (TELiteral (TLInt 2)))))
                                                        )
                                                      (TEVar (Variable "myList"))]

multipleExprSpecs :: Spec
multipleExprSpecs = describe "Simple tests of multiple expressions" $ do
  context "when given multiple expressions" $ do
    it "Should turn '1 + 2.\\n3 + 4' into two TEApplys" $
      getAstFromParser "1 + 2.\n3 + 4" `shouldBe` [TEApply (TEApply (TEVar (Variable "+")) (TELiteral (TLInt 1))) (TELiteral (TLInt 2)), TEApply (TEApply (TEVar (Variable "+")) (TELiteral (TLInt 3))) (TELiteral (TLInt 4))]

  context "when given an assignment and an expression" $ do
    it "Should turn 'x :: Integer.\\nx = 5.\\nx + 1' into an TELet and an TEApply" $
      getAstFromParser "x :: Integer.\nx = 5.\nx + 1" `shouldBe`
        [
          TETypeSignature (Variable "x") [] (mkTCon "Integer"),
          TELet (Variable "x") (TELiteral (TLInt 5)),
          TEApply (TEApply (TEVar (Variable "+")) (TEVar (Variable "x"))) (TELiteral (TLInt 1))
        ]

simpleLambdaSpecs :: Spec
simpleLambdaSpecs = describe "Simple tests of lambda expressions" $ do
  context "when given a lambda expression" $ do
    it "Should turn \\(i) -> i + i into an TELambda" $
      getAstFromParser "\\(i) -> i + i" `shouldBe`
        [TELambda
          (Just $ TFPSimple (Variable "i"))
          (TEApply
            (TEApply
              (TEVar (Variable "+"))
              (TEVar (Variable "i")))
            (TEVar (Variable "i")))
        ]


    it "Should handle assignment of a lambda to a variable (function definition)" $
      getAstFromParser "f = \\(i) -> i + i" `shouldBe`
        [TELet
          (Variable "f")
          (TELambda
            (Just $ TFPSimple (Variable "i"))
            (TEApply
              (TEApply
                (TEVar (Variable "+"))
                (TEVar (Variable "i")))
              (TEVar (Variable "i"))))
        ]

    it "Should handle a lambda with complex type declaration" $
      getAstFromParser "f = \\((x, y), z) -> x" `shouldBe`
        [TELet
          (Variable "f")
          (TELambda
            (Just $ TFPPattern
              (TPatternTuple [TFPSimple (Variable "x"),
                              TFPSimple (Variable "y")])
            )
            (TELambda (Just $ TFPSimple (Variable "z")) (TEVar (Variable "x"))))
        ]

  context "when given a lambda with pattern params" $ do
    it "Should match a literal as a lambda parameter" $
      getAstFromParser "f = \\(1) -> 2" `shouldBe`
        [TELet
          (Variable "f")
          (TELambda
            (Just $ TFPPattern (TPatternLiteral (TLInt 1)))
            (TELiteral (TLInt 2)))
        ]
    it "Should match an empty list as a lambda parameter" $
      getAstFromParser "f = \\([]) -> 2" `shouldBe`
        [TELet
          (Variable "f")
          (TELambda
            (Just $ TFPPattern (TPatternList []))
            (TELiteral (TLInt 2)))
        ]

    it "Should match a tuple as a lambda parameter" $
      getAstFromParser "f = \\((1, x)) -> x" `shouldBe`
        [TELet
          (Variable "f")
          (TELambda
            (Just $ TFPPattern (TPatternTuple [TFPPattern (TPatternLiteral (TLInt 1)), TFPSimple (Variable "x")]))
            (TEVar (Variable "x")))
        ]
    it "Should match a list as a lambda parameter" $
      getAstFromParser "head = \\([hd | tl]) -> hd" `shouldBe`
        [TELet
          (Variable "head")
          (TELambda
            (Just $ TFPPattern (TPatternList [TFPSimple (Variable "hd"),
                                      TFPSimple (Variable "tl")]))
            (TEVar (Variable "hd")))
        ]

    it "Should match complex lambda parameters" $
      getAstFromParser "f = \\((1, [hd | tl], x)) -> x" `shouldBe`
        [TELet
          (Variable "f")
          (TELambda
            (Just $ TFPPattern (TPatternTuple [TFPPattern (TPatternLiteral (TLInt 1)),
                                      TFPPattern (TPatternList [TFPSimple (Variable "hd"),
                                        TFPSimple (Variable "tl")]),
                                      TFPSimple (Variable "x")]))
            (TEVar (Variable "x")))
        ]

--     it "Should handle a single argument case lambda" $ do
--       let prog = [s|
-- \|(Just x) -> x + 10
--  |(Nothing) -> 0.
-- |]
--       testParser LP.lambdaP prog `shouldBe` Right
--         (TELambda
--           (Just $ TFPSimple (Variable "__lambdaCaseArg0" Nothing))
--           (TECase
--             (TEVar (Variable "__lambdaCaseArg0" Nothing))
--             [ (TPatternVar (Variable "x"), TEApply (TEApply (TEVar (Variable "+")) (TEVar (Variable "x")) (TELiteral (TLInt 10)))
--             [ (TPatternWildcard, TELiteral (TLInt 0))
--             , (TPatternVar (Variable "x"), TEApply (TEApply (TEVar (Variable "+")) (TEVar (Variable "x")) (TELiteral (TLInt 10)))
--             ])
--           Nothing
--         ))


simpleFunctionCallSpecs :: Spec
simpleFunctionCallSpecs = describe "Simple tests of function calls" $ do
  context "when given a non-parenthized function call" $ do
    it "Should turn f(5) into an TEApply" $
      getAstFromParser "f(5)" `shouldBe` [TEApply (TEVar (Variable "f")) (TELiteral (TLInt 5))]

    it "Should turn f 5 into an TEApply" $
      getAstFromParser "f 5" `shouldBe` [TEApply (TEVar (Variable "f")) (TELiteral (TLInt 5))]

    it "Should handle function calls with multiple aruments" $
      getAstFromParser "f 5 6" `shouldBe` [TEApply (TEApply (TEVar (Variable "f")) (TELiteral (TLInt 5))) (TELiteral (TLInt 6))]

    it "Should handle function calls with multiple variable arguments" $
      getAstFromParser "f a b" `shouldBe` [TEApply (TEApply (TEVar (Variable "f")) (TEVar (Variable "a"))) (TEVar (Variable "b"))]

  context "when given a $ function application" $ do
    it "Should turn f 5 $ g 6" $
      getAstFromParser "f 5 $ g 6" `shouldBe` [TEApply
                                                (TEApply
                                                  (TEVar (Variable "f"))
                                                  (TELiteral (TLInt 5)))
                                                (TEApply
                                                  (TEVar (Variable "g"))
                                                  (TELiteral (TLInt 6)))
                                              ]

simpleProgramSpecs :: Spec
simpleProgramSpecs = describe "Tests of simple programs" $ do
  context "when defining and calling a function" $ do
    it "Should turn two declarations into two expressions" $
     getAstFromParser [s|x = 1.
y = 2|] `shouldBe` [
                      TELet
                        (Variable "x")
                        (TELiteral (TLInt 1)),
                      TELet
                        (Variable "y")
                        (TELiteral (TLInt 2))
                    ]
    it "Should turn a function definition and call into two expressions" $
      getAstFromParser [s|
double = \(i) ->
  i + i.
main = \() -> double 5|] `shouldBe` [
                      TELet
                        (Variable "double")
                        (TELambda
                          (Just $ TFPSimple (Variable "i"))
                          (TEApply
                            (TEApply
                              (TEVar (Variable "+"))
                              (TEVar (Variable "i")))
                            (TEVar (Variable "i")))
                        ),
                      TELet
                        (Variable "main")
                        (TELambda
                          Nothing
                          (TEApply
                            (TEVar (Variable "double"))
                            (TELiteral (TLInt 5)))
                        )
                    ]
    it "Should handle a simple program with imports" $
      getAstFromParser [s|
import Std::List.
import Std::IO (println).
import Std::Math as M.
module MyApp::Main {
  double = \(i) -> add i i.
  main = \() -> {
    num = 5.
    println "5 * 2 = ".
    println (double num).
  }.
}|] `shouldBe` [
      TEImport (ImportStatement (ModuleNameGlobal ["Std", "List"]) Nothing),
      TEImport (ImportStatement (ModuleNameGlobal ["Std", "IO"]) (Just $ FunctionQualified ["println"])),
      TEImport (ImportStatement (ModuleNameGlobal ["Std", "Math"]) (Just $ ModuleQualified "M")),
      TEModule (ModuleNameGlobal ["MyApp", "Main"])
        (TEBlock
          [
            TELet
              (Variable "double")
              (TELambda
                (Just $ TFPSimple (Variable "i"))
                (TEApply
                  (TEApply
                    (TEVar (Variable "add"))
                    (TEVar (Variable "i")))
                  (TEVar (Variable "i")))
              ),
            TELet
              (Variable "main")
              (TELambda
                Nothing
                (TEBlock
                  [
                    TELet (Variable "num") (TELiteral (TLInt 5)),
                    TEApply
                      (TEVar (Variable "println"))
                      (TELiteral (TLString "5 * 2 = ")),
                    TEApply
                      (TEVar (Variable "println"))
                      (TEApply
                        (TEVar (Variable "double"))
                        (TEVar (Variable "num"))
                      )
                  ]
                )
              )
          ]
        )]

typeSpecs :: Spec
typeSpecs = describe "Tests of types" $ do
  context "when given a type signature with a function type" $ do
    it "Should turn x :: Integer -> Integer into an TETypeSignature" $
      getAstFromParser "x :: Integer -> Integer" `shouldBe` [TETypeSignature (Variable "x") [] (TLambda (mkTCon "Integer") (mkTCon "Integer"))]
    it "Should turn x :: Integer -> Integer -> Integer into an TETypeSignature" $
      getAstFromParser "x :: Integer -> Integer -> Integer" `shouldBe` [TETypeSignature (Variable "x") [] (TLambda (TLambda (mkTCon "Integer") (mkTCon "Integer")) (mkTCon "Integer"))]
    it "Should handle infix functions in type signatures" $
      getAstFromParser "(==) :: Integer -> Integer -> Bool" `shouldBe` [TETypeSignature (Variable "==") [] (TLambda (TLambda (mkTCon "Integer") (mkTCon "Integer")) (mkTCon "Bool"))]

  context "when given a type variable in a type signature" $ do
    it "Should turn x :: a -> a into an TETypeSignature" $
      getAstFromParser "x :: a -> a" `shouldBe` [TETypeSignature (Variable "x") [] (TLambda ((TVar . TV) "a") ((TVar . TV) "a"))]

    it "Should handle class constraints in type signatures" $
      getAstFromParser "x :: @{Eq(a), Ord(b), Eq(c)}: a -> b -> c" `shouldBe`
        [TETypeSignature
          (Variable "x")
          [TCon (TC "Eq") [(TVar . TV) "a"], TCon (TC "Ord") [(TVar . TV) "b"], TCon (TC "Eq") [(TVar . TV) "c"]]
          (TLambda (TLambda ((TVar . TV) "a") ((TVar . TV) "b")) ((TVar . TV) "c"))]

  context "when given an assignment to a partial function" $ do
    it "Should turn add5 = add 5 into an TELet" $
      getAstFromParser "add5 = add 5" `shouldBe` [TELet (Variable "add5") (TEApply (TEVar (Variable "add")) (TELiteral (TLInt 5)))]

  context "when given a typeclass declaration" $ do
    it "Should generate an TEClass" $
      testParser LP.typeclassDeclP [s|
class @{Ord(a)}: Eq(a) {
  (==) :: a -> a -> Bool.
}
|] `shouldBe` (Right $ TEClass
                [TCon (TC "Ord") [(TVar . TV) "a"]]
                (TCon (TC "Eq") [(TVar . TV) "a"])
                (TEBlock [TETypeSignature (Variable "==") [] (TLambda (TLambda ((TVar . TV) "a") ((TVar . TV) "a")) (mkTCon "Bool"))])
              )

  context "when given a typeclass implementation" $ do
    it "Should generate an TEClassImpl" $
      getAstFromParser [s|
impl @{Ord(a)}: Eq(Integer) {
  (==) = \(a, b) -> haskellInTEq a b.
}
|] `shouldBe` [
                TEClassImpl
                  [TCon (TC "Ord") [(TVar . TV) "a"]]
                  (TCon (TC "Eq") [mkTCon "Integer"])
                  (TEBlock
                    [TELet
                      (Variable "==")
                      (TELambda
                        (Just $ TFPSimple (Variable "a"))
                        (TELambda
                          (Just $ TFPSimple (Variable "b"))
                          (TEApply
                            (TEApply
                              (TEVar (Variable "haskellInTEq"))
                              (TEVar (Variable "a")))
                            (TEVar (Variable "b")))))])
              ]
  context "when given data constructors in types" $ do
    it "Should handle a type with a constructor" $
      testParser LP.typeSignatureP "x :: Ordering" `shouldBe` Right (TETypeSignature (Variable "x") [] (TCon (TC "Ordering") []))

    it "Should handle a type with a constructor with type parameters" $
      getAstFromParser "x :: Maybe(Integer)" `shouldBe` [TETypeSignature (Variable "x") [] (TCon (TC "Maybe") [mkTCon "Integer"])]

statementSpecs :: Spec
statementSpecs = describe "Tests of Statements" $ do
  context "when given an import statement" $ do
    it "Should turn 'import Foo' into an TEImport" $
      getAstFromParser "import Foo" `shouldBe` [TEImport (ImportStatement (ModuleNameGlobal ["Foo"]) Nothing)]

    it "Should turn 'import Foo::Bar' into an SImport" $
      getAstFromParser "import Foo::Bar" `shouldBe` [TEImport (ImportStatement (ModuleNameGlobal ["Foo", "Bar"]) Nothing)]

    it "Should turn 'import ::Bar' into an SImport" $
      getAstFromParser "import ::Bar::Baz" `shouldBe` [TEImport (ImportStatement (ModuleNameChild ["Bar", "Baz"]) Nothing)]

    it "Should handle a module qualified import" $
      getAstFromParser "import Foo::Bar as B" `shouldBe` [TEImport (ImportStatement (ModuleNameGlobal ["Foo", "Bar"]) (Just $ ModuleQualified "B"))]

    it "Should handle a functin qualified import" $
      getAstFromParser "import Std::List (map, filter)" `shouldBe` [TEImport (ImportStatement (ModuleNameGlobal ["Std", "List"]) (Just $ FunctionQualified ["map", "filter"]))]

  context "when given a module definition" $ do
    it "Should turn 'module Foo::Bar { x = 1. }' into an TEModule" $
      getAstFromParser "module Foo::Bar { x = 1. }" `shouldBe` [TEModule (ModuleNameGlobal ["Foo", "Bar"]) (TEBlock [TELet (Variable "x") (TELiteral (TLInt 1))])]
    
    it "Should handle nested modules" $
      getAstFromParser "module Foo { module Bar {} }" `shouldBe` [TEModule (ModuleNameGlobal ["Foo"]) (TEBlock [TEModule (ModuleNameGlobal ["Bar"]) (TEBlock [])])]

--   context "when given a case statement" $ do
--     it "Should parse a simple case statement" $ do
--       let prog = [s|
-- case x {
--   | 1 -> 2
--   | 2 -> 3
-- }|]
--       (testParser LP.caseP prog) `shouldBe` Right
--         (TECase
--           (TEVar (Variable "x"))
--           [ (TPatternLiteral (TLInt 1), TELiteral (TLInt 2))
--           , (TPatternLiteral (TLInt 2), TELiteral (TLInt 3))
--           ])

--     -- This would be additional complexity to handle, so we'll just allow it for now, even though it's incredibly confusing to read
--     -- it "Should fail to parse a single line case statement" $
--     --   testParser LP.caseP "case x { | 1 -> 2 | 2 -> 3 }" `shouldSatisfy` isLeft 

--     it "Should fail to parse a single pattern case statement" $ do
--       let prog = [s|
-- case x {
--   | 1 -> 2
-- }|]
--       testParser LP.caseP prog `shouldSatisfy` isLeft 
--     it "Should parse a case statement with a default pattern" $ do
--       let prog = [s|
-- case x {
--   | 1 -> 2
--   | _ -> 3
-- }|]
--       testParser LP.caseP prog `shouldBe` Right
--         (TECase
--           (TEVar (Variable "x"))
--           [ (TPatternLiteral (TLInt 1), TELiteral (TLInt 2))
--           , (TPatternWildcard, TELiteral (TLInt 3))
--           ])
--     it "Should parse a case statement with block results" $ do
--       let prog = [s|
-- case x {
--   | 1 -> {
--     y = 2.
--     y
--   }
--   | 2 -> 3
-- }|]    
--       testParser LP.caseP prog `shouldBe` Right
--         (TECase
--           (TEVar (Variable "x"))
--           [
--             (TPatternLiteral
--               (TLInt 1),
--               TEBlock [TELet (Variable "y") (TELiteral (TLInt 2)), TEVar (Variable "y")])
--           , (TPatternLiteral (TLInt 2), TELiteral (TLInt 3))
--           ])


constructorSpecs :: Spec
constructorSpecs = describe "Tests of constructors" $ do
  context "when declaring a constructor" $ do
    it "Should handle an enum-style constructor" $
      testParser LP.dataDeclP "data Ordering { LT, EQ, GT }" `shouldBe` Right (TEData "Ordering" [] [Constructor "LT" [], Constructor "EQ" [], Constructor "GT" []])

    it "Should handle a constructor with type parameters" $
      testParser LP.dataDeclP "data Maybe(a) { Just(a), Nothing }" `shouldBe` Right (TEData "Maybe" [(TVar . TV) "a"] [Constructor "Just" [(TVar . TV) "a"], Constructor "Nothing" []])

    it "Should handle recursive data types" $
      testParser LP.dataDeclP "data List(a) { Empty, Cons(a, List(a)) }" `shouldBe` Right (TEData "List" [(TVar . TV) "a"] [Constructor "Empty" [], Constructor "Cons" [(TVar . TV) "a", TCon (TC "List") [(TVar . TV) "a"]]])

  context "when using a constructor" $ do
    it "Should handle a constructor as a function" $
      getAstFromParser "x = Just 5" `shouldBe` [TELet (Variable "x") (TEApply (TEVar (Variable "Just")) (TELiteral (TLInt 5)))]

    it "Should handle a constructor with multiple arguments" $
      getAstFromParser "x = Cons 5 Nil" `shouldBe` [TELet (Variable "x") (TEApply (TEApply (TEVar (Variable "Cons")) (TELiteral (TLInt 5))) (TEVar (Variable "Nil")))]

    it "Should handle a constructor with multiple arguments and a type signature" $
      getAstFromParser "x :: List(Integer).\nx = Cons 5 Nil" `shouldBe` [
        TETypeSignature (Variable "x") [] (TCon (TC "List") [mkTCon "Integer"]),
        TELet (Variable "x") (TEApply (TEApply (TEVar (Variable "Cons")) (TELiteral (TLInt 5))) (TEVar (Variable "Nil")))
      ]

    it "Should handle constructor pattern matching" $
      getAstFromParser "head :: List(a) -> a.\nhead = \\(Cons(hd, _)) -> hd" `shouldBe`
        [
          TETypeSignature
            (Variable "head")
            []
            (TLambda (TCon (TC "List") [(TVar . TV) "a"]) ((TVar . TV) "a")),
          TELet
            (Variable "head")
            (TELambda
              (Just $ TFPPattern (TPatternConstructor
                "Cons"
                [TFPSimple (Variable "hd"), TFPPattern TPatternWildcard]
              ))
              (TEVar (Variable "hd")))
        ]

-- class Eq a {
--   (==) = \(_: a, _: a): Bool -> _
-- }
-- class Foldable t {
--   foldl = \(fun: (a -> b -> b), init: b, foldable: t a): b
--   foldl = \(fun: \(_: a, _: b): a, init: b, foldable: t a): b
-- }



-- TODO:
-- FIX THE "." OPERATOR BC IT MAKES A BUNCH OF TESTS FAIL
-- multi-head functions in interpreter
-- polymorphic types
-- make prelude with folds
-- typeclasses
-- ADTs
-- need magic hashes that result in directly calling haskell/native code
-- treat data constructors as functions so that I can use "(Just) 5"
-- need deriving for data constructors
-- need to create functions for constructions of data constructors in analysis pass

-- intentSpecs :: Spec
-- intentSpecs = describe "Tests of indentation" $ do
--   context "when indenting the body of a top level lambda" $ do
--     it "Should accept the entire body of a TL lambda on a new line as long as it's indented"