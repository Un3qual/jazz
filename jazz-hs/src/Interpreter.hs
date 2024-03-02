module Interpreter where

data Value
  = VInt Integer
--   | VFloat Double
--   | VBool Bool
--   | VString String
--   -- | VList [Value]
--   -- | VTuple [Value]
--   | VLambda ([Value] -> Value)
--   -- | VConstructor String [Value]
--   deriving (Show, Eq)
-- simpleEvalExpr :: Expr -> Maybe 
-- simpleEvalExpr (ELiteral lit) = Just $ evalLiteral lit
-- -- simpleEvalExpr (EVar var) = Just $ evalVar var
-- simpleEvalExpr (EApply operator operand) = Just $ evalApply operator operand
-- simpleEvalExpr _ = Nothing


-- evalLiteral :: Literal -> Value
-- evalLiteral (LInt i) = VInt i
-- evalLiteral (LFloat f) = VFloat f
-- evalLiteral (LBool b) = VBool b
-- evalLiteral (LString s) = VString s

-- evalApply :: Expr -> Expr -> Value
-- evalApply Variable (Variable + _) (Variable _ (Just TInt)) = VInt 0
-- -- evalVar :: Variable -> Value
-- -- evalVar (Variable _ (Just TInt)) = VInt 0