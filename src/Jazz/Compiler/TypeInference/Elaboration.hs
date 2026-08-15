-- | Opt-in, deliberately narrow typed-core production support.  The ordinary
-- inference path does not retain these values; they are used only by the
-- explicit resolved-module producer.
module Jazz.Compiler.TypeInference.Elaboration
  ( TypedCoreProductionStatus (..),
    TypedCoreProductionOutcome,
    TypedCoreProductionFailure (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionMode (..),
    InferredProductionFailure (..),
    InferredExpr (..),
    ProvisionalCallableDeclaration (..),
    ProvisionalPatternCaseArm (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    blockProductionFailureKindAndDetail,
    blockedTypedCoreProductionOutcome,
    expressionDependencyNames,
    specializeInferredExpression,
    finalizeValidatedTypedCoreExpressionDirectCall,
    isTypedCoreDirectCallOperator,
    typedCoreProductionOutcomeStatus,
    typedCoreProductionOutcomeValidatedProgram,
    unsupportedTypedCoreProductionOutcome,
  )
where

import qualified Data.Set as Set
import Jazz.Compiler.AST (CaseArm (..), Expr (..), ImplMethod (..), Pattern (..), Statement (..))
import Jazz.Compiler.Name
  ( Name,
    operatorBindingName,
  )
import Jazz.Compiler.Parser.Operator (isBuiltinOperatorSymbol)
import Jazz.Compiler.TypeInference.Elaboration.Finalize
  ( finalizeValidatedTypedCoreExpressionDirectCall,
    isTypedCoreDirectCallOperator,
  )
import Jazz.Compiler.TypeInference.Elaboration.Specialize
  ( specializeInferredExpression,
  )
import Jazz.Compiler.TypeInference.Elaboration.Types
  ( InferredExpr (..),
    InferredProductionFailure (..),
    ProvisionalCallableDeclaration (..),
    ProvisionalPatternCaseArm (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionMode (..),
    TypedCoreProductionOutcome,
    TypedCoreProductionPath (..),
    TypedCoreProductionStatus (..),
    blockProductionFailureKindAndDetail,
    blockedTypedCoreProductionOutcome,
    typedCoreProductionOutcomeStatus,
    typedCoreProductionOutcomeValidatedProgram,
    unsupportedTypedCoreProductionOutcome,
  )

-- | Canonical free value references for dependency analysis. This walks the
-- resolved core expression rather than the provisional production tree, so a
-- rejected expression cannot erase dependency evidence. Scope separately
-- transports canonical recursive-group membership after applying declaration
-- position, rebinding, outer-binding, and lexical-shadow semantics.
expressionDependencyNames :: Expr -> Set.Set Name
expressionDependencyNames = go
  where
    go expression =
      case expression of
        ELit {} -> Set.empty
        EVar name -> Set.singleton name
        ELambda parameterName body -> Set.delete parameterName (go body)
        EOperatorValue operatorSymbol -> operatorDependencies operatorSymbol
        EList elements -> foldMap go elements
        ETuple elements -> foldMap go elements
        EApply function argument -> go function <> go argument
        ETypeApplication function _ _ -> go function
        EIf condition thenExpression elseExpression ->
          go condition <> go thenExpression <> go elseExpression
        EPatternCase scrutinee arms -> go scrutinee <> foldMap armDependencies arms
        EBinary operatorSymbol left right ->
          operatorDependencies operatorSymbol <> go left <> go right
        ESectionLeft left operatorSymbol -> operatorDependencies operatorSymbol <> go left
        ESectionRight operatorSymbol right -> operatorDependencies operatorSymbol <> go right
        EBlock statements -> blockDependencies Set.empty statements
    armDependencies (CaseArm patternValue maybeGuard result) =
      let boundNames = patternBindingNames patternValue
       in (maybe Set.empty go maybeGuard <> go result) Set.\\ boundNames
    methodDependencies (ImplMethod _ _ body) = go body
    blockDependencies _ [] = Set.empty
    blockDependencies lexicalNames (statement : rest) =
      case statement of
        SLet name _ initializer ->
          (go initializer Set.\\ lexicalNames)
            <> blockDependencies (Set.insert name lexicalNames) rest
        SExpr _ result ->
          (go result Set.\\ lexicalNames) <> blockDependencies lexicalNames rest
        SImpl _ _ _ methods ->
          (foldMap methodDependencies methods Set.\\ lexicalNames)
            <> blockDependencies lexicalNames rest
        _ -> blockDependencies lexicalNames rest
    patternBindingNames patternValue =
      case patternValue of
        PWildcard -> Set.empty
        PVariable name -> Set.singleton name
        PLiteral {} -> Set.empty
        PConstructor _ fields -> foldMap patternBindingNames fields
        PList elements -> foldMap patternBindingNames elements
        PConsList headPattern tailPattern -> patternBindingNames headPattern <> patternBindingNames tailPattern
        PTuple elements -> foldMap patternBindingNames elements
        PAs name nested -> Set.insert name (patternBindingNames nested)
        POr alternatives -> foldMap patternBindingNames alternatives
    operatorDependencies operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol = Set.empty
      | otherwise = Set.singleton (operatorBindingName operatorSymbol)
