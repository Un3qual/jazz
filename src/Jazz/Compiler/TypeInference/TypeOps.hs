-- | Pure type-scheme algebra shared by inference owners.
module Jazz.Compiler.TypeInference.TypeOps
  ( dedupeTypeSchemeConstraints,
    freeTypeVariables,
    freeTypeVariablesInTypeSchemeConstraints,
    freeTypeVariablesInTypeSchemePrimitiveConstraints,
    instantiateTypeSchemeConstraint,
    instantiateTypeSchemePrimitiveConstraint,
    replaceTypeVariables,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType,
    InferenceVariable,
    SemanticType (..),
    TypeSchemeConstraint,
    TypeSchemePrimitiveConstraint,
  )
import Jazz.Compiler.TypeRepresentation (substituteSemanticVariables)

dedupeTypeSchemeConstraints :: [TypeSchemeConstraint] -> [TypeSchemeConstraint]
dedupeTypeSchemeConstraints constraints =
  snd (foldl' insertIfMissing (Set.empty, []) (reverse constraints))
  where
    insertIfMissing result@(seen, _) constraint
      | Set.member constraint seen = result
    insertIfMissing (seen, deduplicated) constraint =
      (Set.insert constraint seen, constraint : deduplicated)

freeTypeVariablesInTypeSchemeConstraints :: [TypeSchemeConstraint] -> Set InferenceVariable
freeTypeVariablesInTypeSchemeConstraints = foldMap (foldMap freeTypeVariables)

freeTypeVariablesInTypeSchemePrimitiveConstraints :: [TypeSchemePrimitiveConstraint] -> Set InferenceVariable
freeTypeVariablesInTypeSchemePrimitiveConstraints = foldMap (foldMap freeTypeVariables)

freeTypeVariables :: ExpressionType -> Set InferenceVariable
freeTypeVariables = foldMap Set.singleton

replaceTypeVariables :: Map InferenceVariable ExpressionType -> ExpressionType -> ExpressionType
replaceTypeVariables replacements =
  substituteSemanticVariables (\variable -> Map.findWithDefault (SemanticVariable variable) variable replacements)

instantiateTypeSchemeConstraint :: Map InferenceVariable ExpressionType -> TypeSchemeConstraint -> TypeSchemeConstraint
instantiateTypeSchemeConstraint replacements = fmap (replaceTypeVariables replacements)

instantiateTypeSchemePrimitiveConstraint :: Map InferenceVariable ExpressionType -> TypeSchemePrimitiveConstraint -> TypeSchemePrimitiveConstraint
instantiateTypeSchemePrimitiveConstraint replacements = fmap (replaceTypeVariables replacements)
