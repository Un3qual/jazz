-- | Pure type-scheme algebra shared by inference owners.
module Jazz.Compiler.TypeInference.TypeOps
  ( dedupeTypeSchemeConstraints,
    freeTypeVariables,
    freeTypeVariablesInTypeSchemeConstraints,
    freeTypeVariablesInTypeSchemePrimitiveConstraints,
    instantiateTypeSchemeConstraint,
    instantiateTypeSchemePrimitiveConstraint,
    replaceTypeVariables
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType (..),
    TypeSchemeConstraint (..),
    TypeSchemePrimitiveConstraint (..)
  )

dedupeTypeSchemeConstraints :: [TypeSchemeConstraint] -> [TypeSchemeConstraint]
dedupeTypeSchemeConstraints constraints =
  snd (foldl' insertIfMissing (Set.empty, []) (reverse constraints))
  where
    insertIfMissing result@(seen, _) constraint
      | Set.member constraint seen = result
    insertIfMissing (seen, deduplicated) constraint =
      (Set.insert constraint seen, constraint : deduplicated)

freeTypeVariablesInTypeSchemeConstraints :: [TypeSchemeConstraint] -> Set Int
freeTypeVariablesInTypeSchemeConstraints constraints =
  Set.unions (map freeTypeVariablesInTypeSchemeConstraint constraints)

freeTypeVariablesInTypeSchemeConstraint :: TypeSchemeConstraint -> Set Int
freeTypeVariablesInTypeSchemeConstraint constraint =
  case constraint of
    TypeSchemeConstraint _ argumentType ->
      freeTypeVariables argumentType
    TypeSchemeInferredConstraint _ argumentType ->
      freeTypeVariables argumentType
    TypeSchemeMethodConstraint _ _ argumentType ->
      freeTypeVariables argumentType

freeTypeVariablesInTypeSchemePrimitiveConstraints :: [TypeSchemePrimitiveConstraint] -> Set Int
freeTypeVariablesInTypeSchemePrimitiveConstraints primitiveConstraints =
  Set.unions (map freeTypeVariablesInTypeSchemePrimitiveConstraint primitiveConstraints)

freeTypeVariablesInTypeSchemePrimitiveConstraint :: TypeSchemePrimitiveConstraint -> Set Int
freeTypeVariablesInTypeSchemePrimitiveConstraint primitiveConstraint =
  case primitiveConstraint of
    TypeSchemeNumericConstraint _ argumentType -> freeTypeVariables argumentType
    TypeSchemeStrictEqualityConstraint argumentType -> freeTypeVariables argumentType

freeTypeVariables :: ExpressionType -> Set Int
freeTypeVariables expressionType =
  case expressionType of
    TIntType -> Set.empty
    TIntegerLiteralType {} -> Set.empty
    TFloatType -> Set.empty
    TNumericType {} -> Set.empty
    TBoolType -> Set.empty
    TCharType -> Set.empty
    TTextType -> Set.empty
    TListType elementType ->
      freeTypeVariables elementType
    TTupleType elementTypes ->
      Set.unions (map freeTypeVariables elementTypes)
    TDataType _ typeArguments ->
      Set.unions (map freeTypeVariables typeArguments)
    TFunctionType inputType outputType ->
      Set.union (freeTypeVariables inputType) (freeTypeVariables outputType)
    TVarType typeVar ->
      Set.singleton typeVar

replaceTypeVariables :: Map Int ExpressionType -> ExpressionType -> ExpressionType
replaceTypeVariables replacements expressionType =
  case expressionType of
    TIntType -> TIntType
    TIntegerLiteralType literalRange -> TIntegerLiteralType literalRange
    TFloatType -> TFloatType
    TNumericType numericType -> TNumericType numericType
    TBoolType -> TBoolType
    TCharType -> TCharType
    TTextType -> TTextType
    TListType elementType ->
      TListType (replaceTypeVariables replacements elementType)
    TTupleType elementTypes ->
      TTupleType (map (replaceTypeVariables replacements) elementTypes)
    TDataType typeName typeArguments ->
      TDataType typeName (map (replaceTypeVariables replacements) typeArguments)
    TFunctionType inputType outputType ->
      TFunctionType
        (replaceTypeVariables replacements inputType)
        (replaceTypeVariables replacements outputType)
    TVarType typeVar ->
      Map.findWithDefault expressionType typeVar replacements

instantiateTypeSchemeConstraint :: Map Int ExpressionType -> TypeSchemeConstraint -> TypeSchemeConstraint
instantiateTypeSchemeConstraint replacements constraint =
  case constraint of
    TypeSchemeConstraint constraintName argumentType ->
      TypeSchemeConstraint constraintName (replaceTypeVariables replacements argumentType)
    TypeSchemeInferredConstraint constraintName argumentType ->
      TypeSchemeInferredConstraint constraintName (replaceTypeVariables replacements argumentType)
    TypeSchemeMethodConstraint constraintName methodKey argumentType ->
      TypeSchemeMethodConstraint constraintName methodKey (replaceTypeVariables replacements argumentType)

instantiateTypeSchemePrimitiveConstraint :: Map Int ExpressionType -> TypeSchemePrimitiveConstraint -> TypeSchemePrimitiveConstraint
instantiateTypeSchemePrimitiveConstraint replacements primitiveConstraint =
  case primitiveConstraint of
    TypeSchemeNumericConstraint numericConstraint argumentType ->
      TypeSchemeNumericConstraint numericConstraint (replaceTypeVariables replacements argumentType)
    TypeSchemeStrictEqualityConstraint argumentType ->
      TypeSchemeStrictEqualityConstraint (replaceTypeVariables replacements argumentType)
