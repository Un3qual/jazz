-- | Free-variable summaries for type environments and binding generalization.
module Jazz.Compiler.TypeInference.Environment
  ( freeTypeVariablesInEnv,
    TypeEnvFreeVariables,
    typeEnvFreeVariables,
    insertTypeEnvFreeVariables,
    deleteTypeEnvFreeVariables,
    resolveTypeEnvFreeVariables,
    insertResolvedTypeBinding,
    insertResolvedTypeEnvFreeVariables,
  )
where

import Data.Map.Strict
  ( Map,
  )
import qualified Data.Map.Strict as Map
import Data.Set
  ( Set,
  )
import qualified Data.Set as Set
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts (resolvedNodeShadowedReference))
import Jazz.Compiler.Name
  ( ResolvedName,
  )
import Jazz.Compiler.TypeInference.Solver
  ( resolveType,
  )
import Jazz.Compiler.TypeInference.State
  ( InferState (..),
  )
import Jazz.Compiler.TypeInference.TypeOps
  ( freeTypeVariables,
    freeTypeVariablesInTypeSchemeConstraints,
    freeTypeVariablesInTypeSchemePrimitiveConstraints,
  )
import Jazz.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    InferenceVariable,
    SemanticType (..),
    TypeBinding (..),
    TypeEnv,
    TypeEnvKey (..),
    TypeScheme (..),
    quantifiedVariablesMembershipSet,
    typeEnvBindingKey,
  )

freeTypeVariablesInEnv :: InferState -> TypeEnv -> Set InferenceVariable
freeTypeVariablesInEnv state =
  Set.unions . map (freeTypeVariablesInBinding state) . Map.elems

data TypeEnvFreeVariables = TypeEnvFreeVariables
  { typeEnvBindingFreeVariables :: Map TypeEnvKey (Set InferenceVariable),
    typeEnvFreeVariableReferenceCounts :: Map InferenceVariable Int
  }

typeEnvFreeVariables :: TypeEnv -> TypeEnvFreeVariables
typeEnvFreeVariables =
  Map.foldlWithKey' (\summary name binding -> insertTypeEnvFreeVariables name binding summary) emptyTypeEnvFreeVariables

emptyTypeEnvFreeVariables :: TypeEnvFreeVariables
emptyTypeEnvFreeVariables = TypeEnvFreeVariables Map.empty Map.empty

insertTypeEnvFreeVariables :: TypeEnvKey -> TypeBinding -> TypeEnvFreeVariables -> TypeEnvFreeVariables
insertTypeEnvFreeVariables name binding summary =
  TypeEnvFreeVariables
    { typeEnvBindingFreeVariables =
        Map.insert name newVariables (typeEnvBindingFreeVariables summary),
      typeEnvFreeVariableReferenceCounts =
        Set.foldl' incrementReference countsWithoutPriorBinding newVariables
    }
  where
    newVariables = freeTypeVariablesInBindingRaw binding
    priorVariables =
      Map.findWithDefault Set.empty name (typeEnvBindingFreeVariables summary)
    countsWithoutPriorBinding =
      Set.foldl' decrementTypeEnvFreeVariableReference (typeEnvFreeVariableReferenceCounts summary) priorVariables
    incrementReference counts typeVar = Map.insertWith (+) typeVar 1 counts

deleteTypeEnvFreeVariables :: TypeEnvKey -> TypeEnvFreeVariables -> TypeEnvFreeVariables
deleteTypeEnvFreeVariables name summary =
  TypeEnvFreeVariables
    { typeEnvBindingFreeVariables =
        Map.delete name (typeEnvBindingFreeVariables summary),
      typeEnvFreeVariableReferenceCounts =
        Set.foldl'
          decrementTypeEnvFreeVariableReference
          (typeEnvFreeVariableReferenceCounts summary)
          priorVariables
    }
  where
    priorVariables =
      Map.findWithDefault Set.empty name (typeEnvBindingFreeVariables summary)

insertResolvedTypeBinding :: ResolvedNodeFacts -> ResolvedName -> TypeBinding -> TypeEnv -> TypeEnv
insertResolvedTypeBinding facts name binding =
  Map.insert (typeEnvBindingKey facts name) binding
    . maybe id (Map.delete . (`TypeEnvKey` name)) (resolvedNodeShadowedReference facts)

insertResolvedTypeEnvFreeVariables :: ResolvedNodeFacts -> ResolvedName -> TypeBinding -> TypeEnvFreeVariables -> TypeEnvFreeVariables
insertResolvedTypeEnvFreeVariables facts name binding =
  insertTypeEnvFreeVariables (typeEnvBindingKey facts name) binding
    . maybe id (deleteTypeEnvFreeVariables . (`TypeEnvKey` name)) (resolvedNodeShadowedReference facts)

decrementTypeEnvFreeVariableReference :: Map InferenceVariable Int -> InferenceVariable -> Map InferenceVariable Int
decrementTypeEnvFreeVariableReference counts typeVar =
  Map.update decrement typeVar counts
  where
    decrement count
      | count <= 1 = Nothing
      | otherwise = Just (count - 1)

resolveTypeEnvFreeVariables :: InferState -> TypeEnvFreeVariables -> Set InferenceVariable
resolveTypeEnvFreeVariables state summary =
  Set.unions
    [ freeTypeVariables (resolveType state (SemanticVariable typeVar))
    | typeVar <- Map.keys (typeEnvFreeVariableReferenceCounts summary)
    ]

freeTypeVariablesInBinding :: InferState -> TypeBinding -> Set InferenceVariable
freeTypeVariablesInBinding state binding =
  Set.unions
    [ freeTypeVariables (resolveType state (SemanticVariable typeVar))
    | typeVar <- Set.toList (freeTypeVariablesInBindingRaw binding)
    ]

freeTypeVariablesInBindingRaw :: TypeBinding -> Set InferenceVariable
freeTypeVariablesInBindingRaw binding =
  case binding of
    PlainTypeBinding expressionType ->
      freeTypeVariables expressionType
    SchemeTypeBinding typeScheme ->
      freeTypeVariablesInSchemeRaw typeScheme
    OperatorAliasSchemeTypeBinding _ typeScheme ->
      freeTypeVariablesInSchemeRaw typeScheme
    BuiltinAliasTypeBinding {} -> Set.empty
    BuiltinOperatorAliasTypeBinding {} -> Set.empty
    ConstructorTypeBinding _ _ argumentTypes ->
      Set.unions (map freeTypeVariablesInConstructorArgumentRaw argumentTypes)

freeTypeVariablesInSchemeRaw :: TypeScheme -> Set InferenceVariable
freeTypeVariablesInSchemeRaw typeScheme =
  Set.difference
    ( Set.unions
        [ freeTypeVariables (schemeResultType typeScheme),
          freeTypeVariablesInTypeSchemeConstraints (schemeClassConstraints typeScheme),
          freeTypeVariablesInTypeSchemePrimitiveConstraints (schemePrimitiveConstraints typeScheme)
        ]
    )
    (quantifiedVariablesMembershipSet (schemeQuantifiedVariables typeScheme))

freeTypeVariablesInConstructorArgumentRaw :: ConstructorArgumentType -> Set InferenceVariable
freeTypeVariablesInConstructorArgumentRaw argumentType =
  case argumentType of
    ConstructorArgumentMonomorphic expressionType -> freeTypeVariables expressionType
    ConstructorArgumentParameter {} -> Set.empty
    ConstructorArgumentStructured {} -> Set.empty
    ConstructorArgumentFresh -> Set.empty
