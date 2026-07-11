{-# LANGUAGE OverloadedStrings #-}

-- | First-order type unification and solver-state constraints.
module JazzNext.Compiler.TypeInference.Solver
  ( addNumericTypeVarConstraint,
    addStrictEqualityTypeVarConstraint,
    applySubstitution,
    bindTypeVar,
    combineIntegerLiteralRanges,
    constrainNumericOperatorType,
    freshTypeVar,
    freshTypeVariable,
    integerLiteralRangeBounds,
    integerLiteralRangeFitsNumericType,
    occursInType,
    resolveType,
    supportsRuntimeEqualityType,
    typeSatisfiesNumericConstraint,
    unifyTypeLists,
    unifyTypes
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST (NumericType (..))
import JazzNext.Compiler.BuiltinCatalog
  ( numericTypeIntegerBounds,
    numericTypeIsIntegral,
    numericTypeSupportsRuntimeArithmetic,
    numericTypeSupportsRuntimeComparison
  )
import JazzNext.Compiler.Name (Name, identifierText)
import JazzNext.Compiler.TypeInference.State
  ( InferState (..),
    SolverState (..),
    inferDataTypes,
    inferNextTypeVar,
    inferNumericVars,
    inferRigidTypeVars,
    inferStrictEqualityVars,
    inferSubst
  )
import JazzNext.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType (..),
    IntegerLiteralRange (..),
    NumericConstraint (..)
  )

freshTypeVar :: InferState -> (ExpressionType, InferState)
freshTypeVar state =
  let (_, expressionType, nextState) = freshTypeVariable state
   in (expressionType, nextState)

freshTypeVariable :: InferState -> (Int, ExpressionType, InferState)
freshTypeVariable state =
  let nextVar = inferNextTypeVar state
   in
    ( nextVar,
      TVarType nextVar,
      modifySolverState
        (\solver -> solver {solverNextTypeVar = nextVar + 1})
        state
    )

resolveType :: InferState -> ExpressionType -> ExpressionType
resolveType state = applySubstitution (inferSubst state)

applySubstitution :: Map Int ExpressionType -> ExpressionType -> ExpressionType
applySubstitution substitution expressionType =
  case expressionType of
    TIntType -> TIntType
    TIntegerLiteralType literalRange -> TIntegerLiteralType literalRange
    TFloatType -> TFloatType
    TNumericType numericType -> TNumericType numericType
    TBoolType -> TBoolType
    TCharType -> TCharType
    TTextType -> TTextType
    TListType elementType -> TListType (applySubstitution substitution elementType)
    TTupleType elementTypes -> TTupleType (map (applySubstitution substitution) elementTypes)
    TDataType typeName typeArguments ->
      TDataType typeName (map (applySubstitution substitution) typeArguments)
    TFunctionType inputType outputType ->
      TFunctionType
        (applySubstitution substitution inputType)
        (applySubstitution substitution outputType)
    TVarType typeVar ->
      case Map.lookup typeVar substitution of
        Just replacementType -> applySubstitution substitution replacementType
        Nothing -> TVarType typeVar

unifyTypes :: ExpressionType -> ExpressionType -> InferState -> Maybe InferState
unifyTypes leftType rightType state =
  let resolvedLeft = resolveType state leftType
      resolvedRight = resolveType state rightType
   in case (resolvedLeft, resolvedRight) of
        (TIntType, TIntType) -> Just state
        (TIntegerLiteralType {}, TIntegerLiteralType {}) -> Just state
        (TIntegerLiteralType {}, TIntType) -> Just state
        (TIntType, TIntegerLiteralType {}) -> Just state
        (TIntegerLiteralType literalRange, TNumericType rightNumericType)
          | integerLiteralRangeFitsNumericType literalRange rightNumericType -> Just state
        (TNumericType leftNumericType, TIntegerLiteralType literalRange)
          | integerLiteralRangeFitsNumericType literalRange leftNumericType -> Just state
        (TFloatType, TFloatType) -> Just state
        (TFloatType, TNumericType NumericFloat64) -> Just state
        (TNumericType NumericFloat64, TFloatType) -> Just state
        (TIntType, TNumericType NumericInt64) -> Just state
        (TNumericType NumericInt64, TIntType) -> Just state
        (TNumericType leftNumericType, TNumericType rightNumericType)
          | leftNumericType == rightNumericType -> Just state
        (TBoolType, TBoolType) -> Just state
        (TCharType, TCharType) -> Just state
        (TTextType, TTextType) -> Just state
        (TDataType leftName leftArguments, TDataType rightName rightArguments)
          | leftName == rightName,
            length leftArguments == length rightArguments ->
              unifyTypeLists leftArguments rightArguments state
        (TListType leftElementType, TListType rightElementType) ->
          unifyTypes leftElementType rightElementType state
        (TTupleType leftElementTypes, TTupleType rightElementTypes)
          | length leftElementTypes == length rightElementTypes ->
              unifyTypeLists leftElementTypes rightElementTypes state
        ( TFunctionType leftInputType leftOutputType,
          TFunctionType rightInputType rightOutputType
          ) -> do
          stateAfterInput <- unifyTypes leftInputType rightInputType state
          unifyTypes leftOutputType rightOutputType stateAfterInput
        (TVarType leftVar, TVarType rightVar)
          | leftVar == rightVar ->
              Just state
          | Set.member leftVar rigidVariables,
            Set.member rightVar rigidVariables ->
              Nothing
          | Set.member leftVar rigidVariables ->
              bindTypeVar rightVar resolvedLeft state
          | Set.member rightVar rigidVariables ->
              bindTypeVar leftVar resolvedRight state
        (TVarType leftVar, _)
          | Set.member leftVar rigidVariables -> Nothing
          | otherwise -> bindTypeVar leftVar resolvedRight state
        (_, TVarType rightVar)
          | Set.member rightVar rigidVariables -> Nothing
          | otherwise -> bindTypeVar rightVar resolvedLeft state
        _ -> Nothing
  where
    rigidVariables = inferRigidTypeVars state

unifyTypeLists :: [ExpressionType] -> [ExpressionType] -> InferState -> Maybe InferState
unifyTypeLists leftTypes rightTypes state
  | length leftTypes /= length rightTypes = Nothing
  | otherwise = foldl' step (Just state) (zip leftTypes rightTypes)
  where
    step maybeState (leftType, rightType) =
      maybeState >>= unifyTypes leftType rightType

bindTypeVar :: Int -> ExpressionType -> InferState -> Maybe InferState
bindTypeVar typeVar replacementType state
  | replacementType == TVarType typeVar = Just state
  | occursInType typeVar replacementType = Nothing
  | typeVarIsStrictEqualityConstrained
      && not (supportsDeferredEqualityOperandType state replacementType) = Nothing
  | otherwise = do
      nextReplacementType <- constrainedReplacementType
      pure
        ( modifySolverState
            ( \solver ->
                solver
                  { solverSubstitution =
                      Map.insert typeVar nextReplacementType (inferSubst state),
                    solverStrictEqualityVars =
                      nextStrictEqualityVars nextReplacementType
                  }
            )
            (stateAfterNumericConstraint nextReplacementType)
        )
  where
    typeVarIsStrictEqualityConstrained =
      Set.member typeVar (inferStrictEqualityVars state)
    typeVarNumericConstraint = Map.lookup typeVar (inferNumericVars state)
    constrainedReplacementType =
      case typeVarNumericConstraint of
        Just numericConstraint ->
          applyNumericConstraintToReplacement numericConstraint replacementType
        Nothing -> Just replacementType
    strictEqualityVarsWithoutTypeVar =
      Set.delete typeVar (inferStrictEqualityVars state)
    nextStrictEqualityVars nextReplacementType =
      case nextReplacementType of
        TVarType replacementVar
          | typeVarIsStrictEqualityConstrained ->
              Set.insert replacementVar strictEqualityVarsWithoutTypeVar
        _ -> strictEqualityVarsWithoutTypeVar
    numericVarsWithoutTypeVar = Map.delete typeVar (inferNumericVars state)
    stateWithoutNumericTypeVar =
      modifySolverState
        (\solver -> solver {solverNumericVars = numericVarsWithoutTypeVar})
        state
    stateAfterNumericConstraint nextReplacementType =
      case (typeVarNumericConstraint, nextReplacementType) of
        (Just numericConstraint, TVarType replacementVar) ->
          addNumericTypeVarConstraint replacementVar numericConstraint stateWithoutNumericTypeVar
        _ -> stateWithoutNumericTypeVar

occursInType :: Int -> ExpressionType -> Bool
occursInType typeVar expressionType =
  case expressionType of
    TIntType -> False
    TIntegerLiteralType {} -> False
    TFloatType -> False
    TNumericType {} -> False
    TBoolType -> False
    TCharType -> False
    TTextType -> False
    TListType elementType -> occursInType typeVar elementType
    TTupleType elementTypes -> any (occursInType typeVar) elementTypes
    TDataType _ typeArguments -> any (occursInType typeVar) typeArguments
    TFunctionType inputType outputType ->
      occursInType typeVar inputType || occursInType typeVar outputType
    TVarType otherVar -> typeVar == otherVar

addStrictEqualityTypeVarConstraint :: Int -> InferState -> InferState
addStrictEqualityTypeVarConstraint typeVar state =
  modifySolverState
    ( \solver ->
        solver
          { solverStrictEqualityVars =
              Set.insert typeVar (inferStrictEqualityVars state)
          }
    )
    state

addNumericTypeVarConstraint :: Int -> NumericConstraint -> InferState -> InferState
addNumericTypeVarConstraint typeVar numericConstraint state =
  modifySolverState
    ( \solver ->
        solver
          { solverNumericVars =
              Map.insertWith
                combineNumericConstraints
                typeVar
                numericConstraint
                (inferNumericVars state)
          }
    )
    state

combineNumericConstraints :: NumericConstraint -> NumericConstraint -> NumericConstraint
combineNumericConstraints leftConstraint rightConstraint =
  case (leftConstraint, rightConstraint) of
    (IntegralLiteralNumericConstraint leftRange, IntegralLiteralNumericConstraint rightRange) ->
      IntegralLiteralNumericConstraint (combineIntegerLiteralRanges leftRange rightRange)
    (IntegralLiteralNumericConstraint literalRange, _) -> IntegralLiteralNumericConstraint literalRange
    (_, IntegralLiteralNumericConstraint literalRange) -> IntegralLiteralNumericConstraint literalRange
    (IntegralNumericConstraint, _) -> IntegralNumericConstraint
    (_, IntegralNumericConstraint) -> IntegralNumericConstraint
    (RuntimeArithmeticNumericConstraint, _) -> RuntimeArithmeticNumericConstraint
    (_, RuntimeArithmeticNumericConstraint) -> RuntimeArithmeticNumericConstraint
    (RuntimeComparisonNumericConstraint, _) -> RuntimeComparisonNumericConstraint
    (_, RuntimeComparisonNumericConstraint) -> RuntimeComparisonNumericConstraint
    _ -> AnyNumericConstraint

applyNumericConstraintToReplacement :: NumericConstraint -> ExpressionType -> Maybe ExpressionType
applyNumericConstraintToReplacement numericConstraint replacementType =
  case (numericConstraint, replacementType) of
    (IntegralLiteralNumericConstraint constraintRange, TIntegerLiteralType replacementRange) ->
      Just (TIntegerLiteralType (combineIntegerLiteralRanges constraintRange replacementRange))
    _
      | typeSatisfiesNumericConstraint numericConstraint replacementType -> Just replacementType
      | otherwise -> Nothing

constrainNumericOperatorType :: NumericConstraint -> ExpressionType -> InferState -> Maybe InferState
constrainNumericOperatorType numericConstraint expressionType state =
  case resolveType state expressionType of
    TVarType typeVar -> Just (addNumericTypeVarConstraint typeVar numericConstraint state)
    resolvedType
      | typeSatisfiesNumericConstraint numericConstraint resolvedType -> Just state
      | otherwise -> Nothing

typeSatisfiesNumericConstraint :: NumericConstraint -> ExpressionType -> Bool
typeSatisfiesNumericConstraint numericConstraint expressionType =
  case numericConstraint of
    AnyNumericConstraint -> anyNumeric
    RuntimeArithmeticNumericConstraint -> runtimeArithmeticNumeric
    RuntimeComparisonNumericConstraint -> runtimeComparisonNumeric
    IntegralNumericConstraint -> integralNumeric
    IntegralLiteralNumericConstraint literalRange ->
      case expressionType of
        TIntType -> True
        TIntegerLiteralType {} -> True
        TNumericType numericType ->
          numericTypeIsIntegral numericType
            && integerLiteralRangeFitsNumericType literalRange numericType
        TVarType {} -> True
        _ -> False
  where
    anyNumeric =
      case expressionType of
        TIntType -> True
        TIntegerLiteralType {} -> True
        TFloatType -> True
        TNumericType {} -> True
        TVarType {} -> True
        _ -> False
    runtimeArithmeticNumeric =
      case expressionType of
        TIntType -> True
        TIntegerLiteralType {} -> True
        TFloatType -> True
        TNumericType numericType -> numericTypeSupportsRuntimeArithmetic numericType
        TVarType {} -> True
        _ -> False
    runtimeComparisonNumeric =
      case expressionType of
        TIntType -> True
        TIntegerLiteralType {} -> True
        TFloatType -> True
        TNumericType numericType -> numericTypeSupportsRuntimeComparison numericType
        TVarType {} -> True
        _ -> False
    integralNumeric =
      case expressionType of
        TIntType -> True
        TIntegerLiteralType {} -> True
        TNumericType numericType -> numericTypeIsIntegral numericType
        TVarType {} -> True
        _ -> False

integerLiteralRangeFitsNumericType :: IntegerLiteralRange -> NumericType -> Bool
integerLiteralRangeFitsNumericType literalRange numericType =
  case numericTypeIntegerBounds numericType of
    Just (lowerBound, upperBound) ->
      let (literalMin, literalMax) = integerLiteralRangeBounds literalRange
       in literalMin >= lowerBound && literalMax <= upperBound
    Nothing -> False

combineIntegerLiteralRanges :: IntegerLiteralRange -> IntegerLiteralRange -> IntegerLiteralRange
combineIntegerLiteralRanges (IntegerLiteralRange leftMin leftMax) (IntegerLiteralRange rightMin rightMax) =
  IntegerLiteralRange (min leftMin rightMin) (max leftMax rightMax)

integerLiteralRangeBounds :: IntegerLiteralRange -> (Integer, Integer)
integerLiteralRangeBounds (IntegerLiteralRange lower upper) = (lower, upper)

supportsRuntimeEqualityType :: InferState -> ExpressionType -> Bool
supportsRuntimeEqualityType state = supportsRuntimeEqualityTypeWith Set.empty state

supportsRuntimeEqualityTypeWith :: Set.Set Text -> InferState -> ExpressionType -> Bool
supportsRuntimeEqualityTypeWith seenDataTypes state expressionType =
  case resolveType state expressionType of
    TIntType -> True
    TIntegerLiteralType {} -> True
    TFloatType -> True
    TNumericType numericType -> numericTypeSupportsRuntimeComparison numericType
    TBoolType -> True
    TCharType -> True
    TTextType -> True
    TListType elementType -> supportsRuntimeEqualityTypeWith seenDataTypes state elementType
    TTupleType elementTypes -> all (supportsRuntimeEqualityTypeWith seenDataTypes state) elementTypes
    TDataType typeName typeArguments ->
      dataTypeSupportsRuntimeEqualityWith seenDataTypes state typeName typeArguments
    _ -> False

dataTypeSupportsRuntimeEqualityWith :: Set.Set Text -> InferState -> Name -> [ExpressionType] -> Bool
dataTypeSupportsRuntimeEqualityWith seenDataTypes state typeName typeArguments =
  let resolvedTypeArguments = map (resolveType state) typeArguments
      dataTypeKey =
        identifierText typeName
          <> "<"
          <> Text.pack (show resolvedTypeArguments)
          <> ">"
   in
    if Set.member dataTypeKey seenDataTypes
      then True
      else checkUnseen (Set.insert dataTypeKey seenDataTypes) resolvedTypeArguments
  where
    checkUnseen nextSeenDataTypes resolvedTypeArguments =
      case Map.lookup (identifierText typeName) (inferDataTypes state) of
        Just (DataTypeBinding typeParameters constructors)
          | length typeParameters == length resolvedTypeArguments ->
              let typeParameterBindings =
                    Map.fromList (zip (map identifierText typeParameters) resolvedTypeArguments)
               in all
                    (all (constructorArgumentSupportsRuntimeEquality nextSeenDataTypes typeParameterBindings))
                    constructors
        _ -> False

    constructorArgumentSupportsRuntimeEquality nextSeenDataTypes typeParameterBindings argumentType =
      case argumentType of
        ConstructorArgumentMonomorphic expressionType ->
          supportsRuntimeEqualityTypeWith nextSeenDataTypes state expressionType
        ConstructorArgumentParameter parameterName ->
          maybe
            False
            (supportsRuntimeEqualityTypeWith nextSeenDataTypes state)
            (Map.lookup parameterName typeParameterBindings)
        ConstructorArgumentFresh -> False

supportsDeferredEqualityOperandType :: InferState -> ExpressionType -> Bool
supportsDeferredEqualityOperandType state expressionType =
  case resolveType state expressionType of
    TVarType _ -> True
    _ -> supportsRuntimeEqualityType state expressionType

modifySolverState :: (SolverState -> SolverState) -> InferState -> InferState
modifySolverState update state =
  state {inferSolver = update (inferSolver state)}
