module Jazz.Compiler.TypedCore.Query
  ( typedExpressionReferencesAnyBinder,
    typedPatternInfo,
    typedPatternChildren,
  )
where

import qualified Data.Set as Set
import Jazz.Compiler.TypedCore

typedExpressionReferencesAnyBinder :: Set.Set TypedBinderId -> TypedExpr -> Bool
typedExpressionReferencesAnyBinder binders expression =
  case expression of
    TypedLiteralExpr {} -> False
    TypedVariableExpr _ _ binderReference ->
      maybe False (`Set.member` binders) binderReference
    TypedLambdaExpr _ _ _ body -> child body
    TypedOperatorValueExpr {} -> False
    TypedListExpr _ elements -> any child elements
    TypedTupleExpr _ elements -> any child elements
    TypedApplyExpr _ function argument -> child function || child argument
    TypedTypeApplicationExpr _ function _ _ -> child function
    TypedIfExpr _ condition thenExpression elseExpression ->
      any child [condition, thenExpression, elseExpression]
    TypedPatternCaseExpr _ scrutinee arms ->
      child scrutinee || any armReferencesBinder arms
    TypedBinaryExpr _ _ left right -> child left || child right
    TypedLeftSectionExpr _ left _ -> child left
    TypedRightSectionExpr _ _ right -> child right
    TypedBlockExpr _ statements -> any statementReferencesBinder statements
  where
    child = typedExpressionReferencesAnyBinder binders
    armReferencesBinder (TypedCaseArm _ maybeGuard result) =
      maybe False child maybeGuard || child result
    statementReferencesBinder statement =
      case statement of
        TypedLetStatement _ _ _ _ initializer -> child initializer
        TypedExpressionStatement _ result -> child result
        TypedImplStatement (TypedImplDeclaration _ _ methods) ->
          any methodReferencesBinder methods
        _ -> False
    methodReferencesBinder (TypedMethodDefinition _ _ _ _ body) = child body

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
