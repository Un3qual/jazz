{-# LANGUAGE DataKinds #-}

-- | Declaration checking for implementation bodies. Capability lookup and
-- constraint solving remain dependencies; expression inference is supplied by
-- the scope owner so this module does not depend on expression traversal.
module Jazz.Compiler.TypeInference.ImplChecking (checkImplMethodBodies) where

import Control.Monad.Trans.State.Strict (get, modify', put, runState, state)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Void (Void)
import Jazz.Compiler.AST
  ( CoreNode (coreNodeFacts, coreNodeSpan),
    CorePhase (Resolved),
    Expr,
    ImplMethod (..),
  )
import Jazz.Compiler.CapabilityFacts (qualifiedMethodKey)
import Jazz.Compiler.Diagnostics (DiagnosticContext (CheckingImplMethod))
import Jazz.Compiler.Name (ResolvedName, identifierText, qualifiedMemberName)
import Jazz.Compiler.SemanticDeclarations (concreteImplementationType)
import Jazz.Compiler.TypeInference.Capabilities
  ( defaultLiteralTypes,
    finalizeDeferredExplicitConstraintsAt,
    instantiateClassMethodTarget,
  )
import Jazz.Compiler.TypeInference.Diagnostics
  ( addTypeError,
    annotateNewErrorsWithContext,
    mkImplMethodMissingClassMethodError,
    mkImplMethodTypeMismatchError,
  )
import Jazz.Compiler.TypeInference.Solver (resolveType, unifyTypes)
import Jazz.Compiler.TypeInference.State (InferState, inferClassMethodSignatures)
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ExpressionType,
    SemanticType,
    TypeBinding (PlainTypeBinding),
    TypeEnv,
    typeEnvReferenceKey,
  )

checkImplMethodBodies ::
  (TypeEnv -> InferState -> ExpressionType -> Expr 'Resolved -> (result, InferState)) ->
  (result -> Maybe ExpressionType) ->
  TypeEnv ->
  InferState ->
  ResolvedName ->
  [SemanticType ResolvedName Void] ->
  [ImplMethod 'Resolved] ->
  (InferState, [(Int, result)])
checkImplMethodBodies inferExpected resultType env initialState capabilityName arguments methods =
  case arguments of
    [implTarget]
      | concreteImplementationType implTarget,
        not implMethodNamesHaveDuplicates ->
          let (results, finalState) = runState (mapM (checkMethod implTarget) (zip [0 ..] methods)) initialState
           in (finalState, catMaybes results)
    _ -> (initialState, [])
  where
    implMethodNamesHaveDuplicates =
      let methodNames = map (\(ImplMethod _ methodName _) -> identifierText methodName) methods
       in length methodNames /= Set.size (Set.fromList methodNames)

    checkMethod implTarget (methodIndex, ImplMethod methodNode methodName methodExpr) = do
      beforeSignature <- get
      let methodSpan = coreNodeSpan methodNode
          methodKey = qualifiedMethodKey capabilityName methodName
      result <- case Map.lookup methodKey (inferClassMethodSignatures beforeSignature) of
        Nothing -> do
          modify' (\current -> addTypeError current (mkImplMethodMissingClassMethodError methodKey methodSpan))
          pure Nothing
        Just classMethodType -> do
          let ClassMethodType parameter declaredMethodType = classMethodType
              maybeExpectedType = instantiateClassMethodTarget parameter implTarget declaredMethodType
          case maybeExpectedType of
            Nothing -> pure Nothing
            Just expectedType -> do
              -- Keep the constraint checkpoint after signature preparation. A
              -- failed unification must retain the pre-unification state, not
              -- any partial solver substitutions, before checking later bodies.
              beforeBody <- get
              methodResult <- state (\current -> inferExpected (implMethodEnv implTarget beforeSignature) current expectedType methodExpr)
              afterBody <- get
              case resultType methodResult of
                Just methodType ->
                  put $
                    case unifyTypes expectedType methodType afterBody of
                      Just unifiedState -> unifiedState
                      Nothing ->
                        addTypeError
                          afterBody
                          ( mkImplMethodTypeMismatchError
                              methodKey
                              methodSpan
                              (defaultLiteralTypes afterBody (resolveType afterBody expectedType))
                              (defaultLiteralTypes afterBody (resolveType afterBody methodType))
                          )
                Nothing -> pure ()
              modify' (finalizeDeferredExplicitConstraintsAt methodSpan beforeBody)
              pure (Just (methodIndex, methodResult))

      modify' (annotateNewErrorsWithContext (CheckingImplMethod methodKey) methodSpan beforeSignature)
      pure result

    implMethodEnv implTarget stateForBindings =
      Map.union env $
        Map.fromList
          [ (typeEnvReferenceKey (coreNodeFacts node) (qualifiedMemberName capabilityName methodName), PlainTypeBinding methodType)
          | ImplMethod node methodName _ <- methods,
            let methodKey = qualifiedMethodKey capabilityName methodName,
            Just (ClassMethodType classParameter methodSignature) <- [Map.lookup methodKey (inferClassMethodSignatures stateForBindings)],
            Just methodType <- [instantiateClassMethodTarget classParameter implTarget methodSignature]
          ]
