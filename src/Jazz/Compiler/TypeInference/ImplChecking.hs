{-# LANGUAGE DataKinds #-}

-- | Check each implementation body once, with its promised variables rigid.
module Jazz.Compiler.TypeInference.ImplChecking (checkImplMethodBodies, checkImplementationSuperclasses) where

import Control.Monad.Trans.State.Strict (get, modify', put, runState, state)
import qualified Control.Monad.Trans.State.Strict as Trial
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST (CoreNode (coreNodeSpan), CorePhase (Resolved), Expr, ImplMethod (..))
import Jazz.Compiler.CapabilityFacts (qualifiedMethodKey)
import Jazz.Compiler.CoreIdentity (CapabilityId (..), renderCapabilityMethodKey)
import Jazz.Compiler.Diagnostics (DiagnosticContext (CheckingImplMethod))
import Jazz.Compiler.Name (identifierText)
import Jazz.Compiler.TypeInference.Capabilities (capabilityFactsFromState, checkMethodPrimitiveConstraints, defaultLiteralTypes, finalizeBindingConstraintsAt, freshImplementation, resolveCapabilityEvidence)
import Jazz.Compiler.TypeInference.Diagnostics (addTypeError, annotateNewErrorsWithContext, mkImplMethodMissingClassMethodError, mkImplMethodTypeMismatchError)
import Jazz.Compiler.TypeInference.Solver (freshTypeVars, resolveType, unifyTypes)
import Jazz.Compiler.TypeInference.State (InferState (..), SolverState (..), inferClassMethodSignatures, inferRigidTypeVars)
import Jazz.Compiler.TypeInference.Types
  ( ClassDefinition (..),
    ClassMethodType (..),
    ExpressionType,
    ImplementationTemplate (..),
    ScopeCapabilityFacts (..),
    SemanticScheme (..),
    TypeEnv,
    TypeScheme,
    instantiateDeclarationType,
    quantifiedVariablesFromPreferred,
    quantifiedVariablesOrderedList,
  )

checkImplMethodBodies ::
  (TypeEnv -> InferState -> ExpressionType -> Expr 'Resolved -> (result, InferState)) ->
  (result -> Maybe ExpressionType) ->
  TypeEnv ->
  InferState ->
  CapabilityId ->
  SemanticScheme Text ->
  [ImplMethod 'Resolved] ->
  (InferState, [(Int, (TypeScheme, result))])
checkImplMethodBodies inferExpected resultType env initialState (CapabilityId capabilityName) targetScheme methods
  | length methodNames /= Set.size (Set.fromList methodNames) = (initialState, [])
  | otherwise =
      let (results, finalState) = runState (mapM checkMethod (zip [0 ..] methods)) initialState
       in (finalState, catMaybes results)
  where
    methodNames = [identifierText name | ImplMethod _ name _ <- methods]
    checkMethod (methodIndex, ImplMethod methodNode methodName methodExpr) = do
      beforeSignature <- get
      let methodSpan = coreNodeSpan methodNode
          methodKey = qualifiedMethodKey capabilityName methodName
      result <- case Map.lookup methodKey (inferClassMethodSignatures beforeSignature) of
        Nothing -> do
          modify' (\current -> addTypeError current (mkImplMethodMissingClassMethodError (renderCapabilityMethodKey methodKey) methodSpan))
          pure Nothing
        Just (ClassMethodScheme parameter methodScheme) -> do
          let instanceNames = quantifiedVariablesOrderedList (schemeQuantifiedVariables targetScheme)
              localNames = filter (/= parameter) (quantifiedVariablesOrderedList (schemeQuantifiedVariables methodScheme))
          instanceTypes <- state (freshTypeVars (length instanceNames))
          localTypes <- state (freshTypeVars (length localNames))
          let instanceBindings = Map.fromList (zip instanceNames instanceTypes)
              instantiatedTarget = instantiateDeclarationType instanceBindings (schemeResultType targetScheme)
              methodBindings target = Map.insert parameter target (Map.fromList (zip localNames localTypes))
              instantiateMethod target = do
                expected <- instantiateDeclarationType (methodBindings target) (schemeResultType methodScheme)
                methodConstraints <- traverse (traverse (instantiateDeclarationType (methodBindings target))) (schemeClassConstraints methodScheme)
                prerequisites <- traverse (traverse (instantiateDeclarationType instanceBindings)) (schemeClassConstraints targetScheme)
                pure (expected, methodConstraints <> prerequisites)
          case instantiatedTarget >>= instantiateMethod of
            Nothing -> pure Nothing
            Just (expectedType, assumptions) -> do
              beforeBody <- get
              let variables = concatMap toList (instanceTypes <> localTypes)
                  expectedScheme = SemanticScheme (quantifiedVariablesFromPreferred variables (Set.fromList variables)) assumptions [] mempty expectedType
              modify' (\current -> current {inferSolver = (inferSolver current) {solverRigidTypeVars = inferRigidTypeVars current <> Set.fromList variables}})
              methodResult <- state (\current -> inferExpected env current expectedType methodExpr)
              afterBody <- get
              case resultType methodResult of
                Just methodType -> put $ case unifyTypes expectedType methodType afterBody of
                  Just unified -> unified
                  Nothing ->
                    addTypeError
                      afterBody
                      ( mkImplMethodTypeMismatchError
                          (renderCapabilityMethodKey methodKey)
                          methodSpan
                          (defaultLiteralTypes afterBody (resolveType afterBody expectedType))
                          (defaultLiteralTypes afterBody (resolveType afterBody methodType))
                      )
                Nothing -> pure ()
              modify' (checkMethodPrimitiveConstraints (renderCapabilityMethodKey methodKey) methodSpan (Set.fromList variables) assumptions beforeBody)
              modify' (finalizeBindingConstraintsAt methodSpan assumptions Set.empty beforeBody)
              modify' (\current -> current {inferSolver = (inferSolver current) {solverRigidTypeVars = inferRigidTypeVars beforeSignature}})
              pure (Just (methodIndex, (expectedScheme, methodResult)))
      modify' (annotateNewErrorsWithContext (CheckingImplMethod (renderCapabilityMethodKey methodKey)) methodSpan beforeSignature)
      pure result

-- A subclass implementation must provide its parent evidence even when no
-- caller invokes a method. Rigid head parameters prevent a concrete parent
-- instance from satisfying a promise made for every parameter.
checkImplementationSuperclasses :: ImplementationTemplate -> InferState -> InferState
checkImplementationSuperclasses template initialState = case Trial.runStateT (freshImplementation template) initialState of
  Nothing -> initialState
  Just ((target, assumptions), allocated) ->
    let rigid = Set.fromList (toList target)
        before = allocated {inferSolver = (inferSolver allocated) {solverRigidTypeVars = inferRigidTypeVars allocated <> rigid}}
        checked = foldl' (check target assumptions) before parents
     in checked {inferSolver = (inferSolver checked) {solverRigidTypeVars = inferRigidTypeVars initialState}}
  where
    facts = capabilityFactsFromState initialState
    parents = maybe [] classSuperclasses (Map.lookup (implementationCapability template) (scopeClassFacts facts))
    check target assumptions current parent = case resolveCapabilityEvidence assumptions facts parent Nothing target current of
      Left diagnostic -> addTypeError current diagnostic
      Right (_, next) -> next
