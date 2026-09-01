{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Jazz.Compiler.AST (CaseArm (..), CorePhase (..), Expr (..), ImplMethod (..), Pattern (..), Statement (..))
import Jazz.Compiler.Name
  ( ResolvedName,
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
expressionDependencyNames :: Expr 'Resolved -> Set.Set ResolvedName
expressionDependencyNames = go
  where
    go expression =
      case expression of
        ELit {} -> Set.empty
        EVar _ name -> Set.singleton name
        ELambda _ parameterName body -> Set.delete parameterName (go body)
        EOperatorValue _ operatorSymbol -> operatorDependencies operatorSymbol
        EList _ elements -> foldMap go elements
        ETuple _ elements -> foldMap go elements
        EApply _ function argument -> go function <> go argument
        ETypeApplication _ function _ _ -> go function
        EIf _ condition thenExpression elseExpression ->
          go condition <> go thenExpression <> go elseExpression
        EPatternCase _ scrutinee arms -> go scrutinee <> foldMap armDependencies arms
        EBinary _ operatorSymbol left right ->
          operatorDependencies operatorSymbol <> go left <> go right
        ESectionLeft _ left operatorSymbol -> operatorDependencies operatorSymbol <> go left
        ESectionRight _ operatorSymbol right -> operatorDependencies operatorSymbol <> go right
        EBlock _ statements -> blockDependencies Set.empty statements
    armDependencies (CaseArm _ patternValue maybeGuard result) =
      let boundNames = patternBindingNames patternValue
       in (maybe Set.empty go maybeGuard <> go result) Set.\\ boundNames
    methodDependencies (ImplMethod _ _ body) = go body
    blockDependencies _ [] = Set.empty
    blockDependencies lexicalNames (statement : rest) =
      case statement of
        SLet _ name initializer ->
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
        PWildcard _ -> Set.empty
        PVariable _ name -> Set.singleton name
        PLiteral {} -> Set.empty
        PConstructor _ _ fields -> foldMap patternBindingNames fields
        PList _ elements -> foldMap patternBindingNames elements
        PConsList _ headPattern tailPattern -> patternBindingNames headPattern <> patternBindingNames tailPattern
        PTuple _ elements -> foldMap patternBindingNames elements
        PAs _ name nested -> Set.insert name (patternBindingNames nested)
        POr _ alternatives -> foldMap patternBindingNames alternatives
    operatorDependencies operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol = Set.empty
      | otherwise = Set.singleton (operatorBindingName operatorSymbol)
