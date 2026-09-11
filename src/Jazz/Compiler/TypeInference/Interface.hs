-- | Close declarations over stable parameters and allocate importer-local solver
-- variables. Monomorphic parameters retain declaration identity across imports.
module Jazz.Compiler.TypeInference.Interface
  ( closeModuleBindings,
    importBindingTypes,
  )
where

import Control.Monad (foldM)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Jazz.Compiler.CoreIdentity (CoreBinderId)
import Jazz.Compiler.ModuleInterface (ModuleExport, ModuleValueBinding (..))
import Jazz.Compiler.SemanticDeclarations
  ( DeclarationVariable (..),
    SemanticBinding,
    bindingQuantifiedVariables,
    bindingVariableOrder,
    mapBindingTypes,
  )
import Jazz.Compiler.TypeInference.Solver (freshTypeVariable, resolveType)
import Jazz.Compiler.TypeInference.State (InferState (..), ModuleInferenceState (..))
import Jazz.Compiler.TypeInference.Types (SemanticType (..), TypeBinding, TypeEnv, TypeEnvKey)

closeModuleBindings :: InferState -> [(ModuleExport, CoreBinderId, TypeBinding)] -> Map ModuleExport ModuleValueBinding
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
          parameters = Map.union quantified withShared
          parameter variable = parameters Map.! variable
      State.put withShared
      pure (export, ModuleValueBinding binder (mapBindingTypes parameter (fmap parameter) binding))

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
    importBinding binding = do
      parameters <- foldM allocate Map.empty (bindingQuantifiedVariables binding <> bindingVariableOrder binding)
      let parameter variable = parameters Map.! variable
      pure (mapBindingTypes parameter (fmap parameter) binding)

    allocate parameters parameter =
      case Map.lookup parameter parameters of
        Just _ -> pure parameters
        Nothing -> do
          (shared, state) <- State.get
          case Map.lookup parameter shared of
            Just variable -> pure (Map.insert parameter variable parameters)
            Nothing -> do
              let (variable, _, nextState) = freshTypeVariable state
                  nextShared = case parameter of
                    SchemeParameter _ -> shared
                    DeclarationParameter {} -> Map.insert parameter variable shared
              State.put (nextShared, nextState)
              pure (Map.insert parameter variable parameters)
