-- | Small desugaring pass for canonical control-flow forms in the core AST.
module JazzNext.Compiler.Desugar
  ( desugarExpr
  ) where

import JazzNext.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    ImplMethod (..),
    Statement (..)
  )

-- | Canonicalize control-flow nodes without changing statement structure.
-- Parser/lower currently produce `EIf`, but the analysis/type phases operate on
-- the `ECase` form.
desugarExpr :: Expr -> Expr
desugarExpr expr =
  case expr of
    ELit literal -> ELit literal
    EVar name -> EVar name
    ELambda parameterName bodyExpr ->
      ELambda parameterName (desugarExpr bodyExpr)
    EOperatorValue operatorSymbol -> EOperatorValue operatorSymbol
    EList elements -> EList (map desugarExpr elements)
    ETuple elements -> ETuple (map desugarExpr elements)
    EApply functionExpr argumentExpr ->
      EApply (desugarExpr functionExpr) (desugarExpr argumentExpr)
    ETypeApplication functionExpr signatureType ->
      ETypeApplication (desugarExpr functionExpr) signatureType
    EIf conditionExpr thenExpr elseExpr ->
      ECase
        (desugarExpr conditionExpr)
        (desugarExpr thenExpr)
        (desugarExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr ->
      ECase
        (desugarExpr conditionExpr)
        (desugarExpr thenExpr)
        (desugarExpr elseExpr)
    EPatternCase scrutineeExpr caseArms ->
      EPatternCase
        (desugarExpr scrutineeExpr)
        (map desugarCaseArm caseArms)
    EBinary operatorSymbol leftExpr rightExpr ->
      EBinary operatorSymbol (desugarExpr leftExpr) (desugarExpr rightExpr)
    ESectionLeft leftExpr operatorSymbol ->
      ESectionLeft (desugarExpr leftExpr) operatorSymbol
    ESectionRight operatorSymbol rightExpr ->
      ESectionRight operatorSymbol (desugarExpr rightExpr)
    EBlock statements ->
      EBlock (map desugarStatement statements)

-- | Statement-level companion to `desugarExpr`.
desugarStatement :: Statement -> Statement
desugarStatement statement =
  case statement of
    SLet name spanValue valueExpr ->
      SLet name spanValue (desugarExpr valueExpr)
    SSignature name spanValue signatureText ->
      SSignature name spanValue signatureText
    SClass spanValue capabilityName parameters methods ->
      SClass spanValue capabilityName parameters methods
    SImpl spanValue capabilityName arguments methods ->
      SImpl spanValue capabilityName arguments (map desugarImplMethod methods)
    SModule spanValue modulePath ->
      SModule spanValue modulePath
    SImport spanValue modulePath alias importedSymbols ->
      SImport spanValue modulePath alias importedSymbols
    SExpr spanValue expr ->
      SExpr spanValue (desugarExpr expr)

desugarCaseArm :: CaseArm -> CaseArm
desugarCaseArm (CaseArm patternExpr guardExpr bodyExpr) =
  -- Patterns are already canonical core nodes; only guards and arm bodies can
  -- contain desugarable control-flow expressions.
  CaseArm patternExpr (fmap desugarExpr guardExpr) (desugarExpr bodyExpr)

desugarImplMethod :: ImplMethod -> ImplMethod
desugarImplMethod (ImplMethod methodName spanValue methodExpr) =
  ImplMethod methodName spanValue (desugarExpr methodExpr)
