-- | Close declarations over stable parameters and allocate importer-local solver
-- variables. Monomorphic parameters retain declaration identity across imports.
module Jazz.Compiler.TypeInference.Interface
  ( closeModuleBindings,
    importBindingTypes,
  )
where

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Jazz.Compiler.CoreIdentity (ResolvedReference)
import Jazz.Compiler.ModuleInterface (ModuleExport, ModuleValueBinding (..))
import Jazz.Compiler.SemanticDeclarations
  ( DeclarationVariable (..),
    SemanticBinding,
    bindingQuantifiedVariables,
    bindingVariableOrder,
    mapBindingTypes,
    traverseBindingTypes,
  )
import Jazz.Compiler.TypeInference.Solver (freshTypeVariable, resolveType)
import Jazz.Compiler.TypeInference.State (InferState (..), ModuleInferenceState (..))
import Jazz.Compiler.TypeInference.Types (SemanticType (..), TypeBinding, TypeEnv, TypeEnvKey)

closeModuleBindings :: InferState -> [(ModuleExport, ResolvedReference, TypeBinding)] -> Map ModuleExport ModuleValueBinding
closeModuleBindings state bindings =
  Map.fromList (State.evalState (traverse closeBinding bindings) importedParameters)
  where
    importedParameters =
      Map.fromList
        [ (resolved, parameter)
        | (variable, parameter) <- Map.toList (inferenceDeclarationParameters (inferModule state)),
          SemanticVariable resolved <- [resolveType state (SemanticVariable variable)]
        ]
    closeBinding (export, binder, rawBinding) = do
      let binding = mapBindingTypes id (resolveType state) rawBinding
          quantified = Map.fromList (zip (bindingQuantifiedVariables binding) (map SchemeParameter [0 ..]))
      shared <- State.get
      let newVariables = filter (\variable -> Map.notMember variable quantified && Map.notMember variable shared) (bindingVariableOrder binding)
          -- Repeated occurrences reuse the first parameter for the declaration.
          withShared =
            foldl'
              (\allocated (index, variable) -> Map.insertWith (\_ existing -> existing) variable (DeclarationParameter binder index) allocated)
              shared
              (zip [0 ..] newVariables)
          parameter variable = case Map.lookup variable quantified of
            Just value -> pure value
            Nothing -> do
              allocated <- State.get
              case Map.lookup variable allocated of
                Just value -> pure value
                Nothing -> do
                  let value = DeclarationParameter binder (Map.size allocated)
                  State.put (Map.insert variable value allocated)
                  pure value
      State.put withShared
      closed <- traverseBindingTypes parameter (traverse parameter) binding
      pure (export, ModuleValueBinding binder closed)

importBindingTypes :: Map TypeEnvKey (SemanticBinding DeclarationVariable) -> InferState -> (TypeEnv, InferState)
importBindingTypes bindings initialState =
  let (environment, (shared, finalState)) = State.runState (traverse importBinding bindings) (Map.empty, initialState)
   in ( environment,
        finalState
          { inferModule =
              (inferModule finalState)
                { inferenceDeclarationParameters = Map.fromList [(variable, parameter) | (parameter, variable) <- Map.toList shared]
                }
          }
      )
  where
    importBinding binding =
      State.evalStateT (traverseBindingTypes allocate (traverse allocate) binding) Map.empty

    allocate parameter = do
      parameters <- State.get
      case Map.lookup parameter parameters of
        Just variable -> pure variable
        Nothing -> do
          (shared, state) <- lift State.get
          let (variable, nextShared, nextState) = case Map.lookup parameter shared of
                Just existing -> (existing, shared, state)
                Nothing ->
                  let (fresh, _, allocatedState) = freshTypeVariable state
                      allocatedShared = case parameter of
                        SchemeParameter _ -> shared
                        DeclarationParameter {} -> Map.insert parameter fresh shared
                   in (fresh, allocatedShared, allocatedState)
          lift (State.put (nextShared, nextState))
          State.put (Map.insert parameter variable parameters)
          pure variable
