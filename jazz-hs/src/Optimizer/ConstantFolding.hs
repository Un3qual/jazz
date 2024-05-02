module Optimizer.ConstantFolding where


import AST

constantFold :: TypedExpr -> TypedExpr
constantFold annExpr@(Ann annotation expr) = case expr of
  EApply
    (Ann _ (EApply (Ann _ (EVar (Variable "+"))) (Ann _ (ELiteral (Ann _ (LInt x))))))
    (Ann _ (ELiteral (Ann _ (LInt y)))) -> Ann annotation $ ELiteral $ Ann annotation $ LInt (x + y)

  EApply
    (Ann _ (EApply (Ann _ (EVar (Variable "-"))) (Ann _ (ELiteral (Ann _ (LInt x))))))
    (Ann _ (ELiteral (Ann _ (LInt y)))) -> Ann annotation $ ELiteral $ Ann annotation $ LInt (x - y)

  EApply
    (Ann _ (EApply (Ann _ (EVar (Variable "*"))) (Ann _ (ELiteral (Ann _ (LInt x))))))
    (Ann _ (ELiteral (Ann _ (LInt y)))) -> Ann annotation $ ELiteral $ Ann annotation $ LInt (x * y)
  
  ELet var body -> Ann annotation $ ELet var (constantFold body)
  EApply f x -> Ann annotation $ EApply (constantFold f) (constantFold x)
  EBlock exprs -> Ann annotation $ EBlock $ map constantFold exprs
  e -> annExpr
