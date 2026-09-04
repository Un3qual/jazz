module Jazz.Compiler.TypedCore.Query
  ( typedExpressionReferencesAnyBinder,
    typedExpressionChildren,
    typedPatternInfo,
    typedPatternChildren,
  )
where

import qualified Data.Set as Set
import Jazz.Compiler.TypedCore

typedExpressionReferencesAnyBinder :: Set.Set TypedBinderId -> TypedExpr -> Bool
typedExpressionReferencesAnyBinder binders expression =
  case expression of
    TypedVariableExpr _ _ reference -> maybe False (`Set.member` binders) reference
    _ -> any (typedExpressionReferencesAnyBinder binders) (typedExpressionChildren expression)

-- | Immediate expression children in authored order, including statement and
-- case-arm bodies. Pattern binders are not expression references.
typedExpressionChildren :: TypedExpr -> [TypedExpr]
typedExpressionChildren expression = case expression of
  TypedLiteralExpr {} -> []
  TypedVariableExpr {} -> []
  TypedLambdaExpr _ _ _ body -> [body]
  TypedOperatorValueExpr {} -> []
  TypedListExpr _ elements -> elements
  TypedTupleExpr _ elements -> elements
  TypedApplyExpr _ function argument -> [function, argument]
  TypedTypeApplicationExpr _ function _ _ -> [function]
  TypedIfExpr _ condition thenExpression elseExpression -> [condition, thenExpression, elseExpression]
  TypedPatternCaseExpr _ scrutinee arms -> scrutinee : concat [maybe [] (: []) guardExpression <> [body] | TypedCaseArm _ guardExpression body <- arms]
  TypedBinaryExpr _ _ left right -> [left, right]
  TypedLeftSectionExpr _ left _ -> [left]
  TypedRightSectionExpr _ _ right -> [right]
  TypedBlockExpr _ statements -> concatMap statementChildren statements
  where
    statementChildren statement = case statement of
      TypedLetStatement _ _ _ _ initializer -> [initializer]
      TypedExpressionStatement _ result -> [result]
      TypedImplStatement (TypedImplDeclaration _ _ methods) -> [body | TypedMethodDefinition _ _ _ _ body <- methods]
      _ -> []

typedPatternInfo :: TypedPattern -> TypedNodeInfo
typedPatternInfo patternValue =
  case patternValue of
    TypedWildcardPattern info -> info
    TypedVariablePattern info _ _ -> info
    TypedLiteralPattern info _ -> info
    TypedConstructorPattern info _ _ -> info
    TypedListPattern info _ -> info
    TypedConsListPattern info _ _ -> info
    TypedTuplePattern info _ -> info
    TypedAsPattern info _ _ _ -> info
    TypedOrPattern info _ -> info

typedPatternChildren :: TypedPattern -> [TypedPattern]
typedPatternChildren patternValue =
  case patternValue of
    TypedConstructorPattern _ _ children -> children
    TypedListPattern _ children -> children
    TypedConsListPattern _ headPattern tailPattern -> [headPattern, tailPattern]
    TypedTuplePattern _ children -> children
    TypedAsPattern _ _ _ nested -> [nested]
    TypedOrPattern _ alternatives -> alternatives
    _ -> []
