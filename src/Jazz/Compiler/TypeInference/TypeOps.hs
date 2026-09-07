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
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..),
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
freeTypeVariablesInTypeSchemeConstraints constraints =
  Set.unions (map freeTypeVariablesInTypeSchemeConstraint constraints)

freeTypeVariablesInTypeSchemeConstraint :: TypeSchemeConstraint -> Set InferenceVariable
freeTypeVariablesInTypeSchemeConstraint constraint =
  case constraint of
    TypeSchemeConstraint _ argumentType ->
      freeTypeVariables argumentType
    TypeSchemeInferredConstraint _ argumentType ->
      freeTypeVariables argumentType
    TypeSchemeMethodConstraint _ _ argumentType ->
      freeTypeVariables argumentType

freeTypeVariablesInTypeSchemePrimitiveConstraints :: [TypeSchemePrimitiveConstraint] -> Set InferenceVariable
freeTypeVariablesInTypeSchemePrimitiveConstraints primitiveConstraints =
  Set.unions (map freeTypeVariablesInTypeSchemePrimitiveConstraint primitiveConstraints)

freeTypeVariablesInTypeSchemePrimitiveConstraint :: TypeSchemePrimitiveConstraint -> Set InferenceVariable
freeTypeVariablesInTypeSchemePrimitiveConstraint primitiveConstraint =
  case primitiveConstraint of
    TypeSchemeNumericConstraint _ argumentType -> freeTypeVariables argumentType
    TypeSchemeStrictEqualityConstraint argumentType -> freeTypeVariables argumentType

freeTypeVariables :: ExpressionType -> Set InferenceVariable
freeTypeVariables = foldMap Set.singleton

replaceTypeVariables :: Map InferenceVariable ExpressionType -> ExpressionType -> ExpressionType
replaceTypeVariables replacements =
  substituteSemanticVariables (\variable -> Map.findWithDefault (SemanticVariable variable) variable replacements)

instantiateTypeSchemeConstraint :: Map InferenceVariable ExpressionType -> TypeSchemeConstraint -> TypeSchemeConstraint
instantiateTypeSchemeConstraint replacements constraint =
  case constraint of
    TypeSchemeConstraint constraintName argumentType ->
      TypeSchemeConstraint constraintName (replaceTypeVariables replacements argumentType)
    TypeSchemeInferredConstraint constraintName argumentType ->
      TypeSchemeInferredConstraint constraintName (replaceTypeVariables replacements argumentType)
    TypeSchemeMethodConstraint constraintName methodKey argumentType ->
      TypeSchemeMethodConstraint constraintName methodKey (replaceTypeVariables replacements argumentType)

instantiateTypeSchemePrimitiveConstraint :: Map InferenceVariable ExpressionType -> TypeSchemePrimitiveConstraint -> TypeSchemePrimitiveConstraint
instantiateTypeSchemePrimitiveConstraint replacements primitiveConstraint =
  case primitiveConstraint of
    TypeSchemeNumericConstraint numericConstraint argumentType ->
      TypeSchemeNumericConstraint numericConstraint (replaceTypeVariables replacements argumentType)
    TypeSchemeStrictEqualityConstraint argumentType ->
      TypeSchemeStrictEqualityConstraint (replaceTypeVariables replacements argumentType)
