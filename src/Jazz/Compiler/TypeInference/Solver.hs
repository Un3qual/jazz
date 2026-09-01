{-# LANGUAGE OverloadedStrings #-}

-- | First-order type unification and solver-state constraints.
module Jazz.Compiler.TypeInference.Solver
  ( addNumericTypeVarConstraint,
    addStrictEqualityTypeVarConstraint,
    applySubstitution,
    bindTypeVar,
    combineIntegerLiteralRanges,
    constrainNumericOperatorType,
    freshTypeVar,
    freshTypeVariable,
    freshIntegerLiteralType,
    integerLiteralRangeFor,
    integerLiteralRangeBounds,
    integerLiteralRangeFitsNumericType,
    occursInType,
    resolveType,
    supportsRuntimeEqualityType,
    typeSatisfiesNumericConstraint,
    unifyTypeLists,
    unifyTypes,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.BuiltinCatalog
  ( numericTypeIntegerBounds,
    numericTypeIsIntegral,
    numericTypeSupportsRuntimeArithmetic,
    numericTypeSupportsRuntimeComparison,
  )
import Jazz.Compiler.Name (Name, identifierText)
import Jazz.Compiler.TypeInference.State
  ( InferState (..),
    SolverState (..),
    inferDataTypes,
    inferNextTypeVar,
    inferNumericVars,
    inferRigidTypeVars,
    inferStrictEqualityVars,
    inferSubst,
  )
import Jazz.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    DataTypeBinding (..),
    ExpressionType (..),
    InferenceVariable,
    IntegerLiteralRange (..),
    NumericConstraint (..),
    instantiateConstructorFieldType,
  )
import Jazz.Compiler.TypeRepresentation (NumericType (..))

freshTypeVar :: InferState -> (ExpressionType, InferState)
freshTypeVar state =
  let (_, expressionType, nextState) = freshTypeVariable state
   in (expressionType, nextState)

freshTypeVariable :: InferState -> (InferenceVariable, ExpressionType, InferState)
freshTypeVariable state =
  let nextVar = inferNextTypeVar state
   in ( nextVar,
        TVarType nextVar,
        modifySolverState
          (\solver -> solver {solverNextTypeVar = nextVar + 1})
          state
      )

freshIntegerLiteralType :: IntegerLiteralRange -> InferState -> (ExpressionType, InferState)
freshIntegerLiteralType literalRange state =
  let (typeVar, literalType, nextState) = freshTypeVariable state
   in ( literalType,
        addNumericTypeVarConstraint
          typeVar
          (IntegralLiteralNumericConstraint literalRange)
          nextState
      )

integerLiteralRangeFor :: InferState -> ExpressionType -> Maybe IntegerLiteralRange
integerLiteralRangeFor state expressionType =
  case resolveType state expressionType of
    TVarType typeVar -> do
      IntegralLiteralNumericConstraint literalRange <- Map.lookup typeVar (inferNumericVars state)
      pure literalRange
    _ -> Nothing

resolveType :: InferState -> ExpressionType -> ExpressionType
resolveType state = applySubstitution (inferSubst state)

applySubstitution :: Map.Map InferenceVariable ExpressionType -> ExpressionType -> ExpressionType
applySubstitution substitution expressionType =
  case expressionType of
    TIntType -> TIntType
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
  {-# SCC "jazz-stage:constraint-solving" #-}
  unifyTypesWithoutCostCentre leftType rightType state

unifyTypesWithoutCostCentre :: ExpressionType -> ExpressionType -> InferState -> Maybe InferState
unifyTypesWithoutCostCentre leftType rightType state =
  let (resolvedLeft, stateAfterLeft) = dereferenceType state leftType
      (resolvedRight, stateAfterDereference) = dereferenceType stateAfterLeft rightType
   in case (resolvedLeft, resolvedRight) of
        (TIntType, TIntType) -> Just stateAfterDereference
        (TFloatType, TFloatType) -> Just stateAfterDereference
        (TFloatType, TNumericType NumericFloat64) -> Just stateAfterDereference
        (TNumericType NumericFloat64, TFloatType) -> Just stateAfterDereference
        (TIntType, TNumericType NumericInt64) -> Just stateAfterDereference
        (TNumericType NumericInt64, TIntType) -> Just stateAfterDereference
        (TNumericType leftNumericType, TNumericType rightNumericType)
          | leftNumericType == rightNumericType -> Just stateAfterDereference
        (TBoolType, TBoolType) -> Just stateAfterDereference
        (TCharType, TCharType) -> Just stateAfterDereference
        (TTextType, TTextType) -> Just stateAfterDereference
        (TDataType leftName leftArguments, TDataType rightName rightArguments)
          | leftName == rightName ->
              unifyTypeListsWithoutCostCentre leftArguments rightArguments stateAfterDereference
        (TListType leftElementType, TListType rightElementType) ->
          unifyTypesWithoutCostCentre leftElementType rightElementType stateAfterDereference
        (TTupleType leftElementTypes, TTupleType rightElementTypes) ->
          unifyTypeListsWithoutCostCentre leftElementTypes rightElementTypes stateAfterDereference
        ( TFunctionType leftInputType leftOutputType,
          TFunctionType rightInputType rightOutputType
          ) -> do
            stateAfterInput <- unifyTypesWithoutCostCentre leftInputType rightInputType stateAfterDereference
            unifyTypesWithoutCostCentre leftOutputType rightOutputType stateAfterInput
        (TVarType leftVar, TVarType rightVar)
          | leftVar == rightVar ->
              Just stateAfterDereference
          | Set.member leftVar rigidVariables,
            Set.member rightVar rigidVariables ->
              Nothing
          | Set.member leftVar rigidVariables ->
              bindTypeVar rightVar resolvedLeft stateAfterDereference
          | Set.member rightVar rigidVariables ->
              bindTypeVar leftVar resolvedRight stateAfterDereference
        (TVarType leftVar, _)
          | Set.member leftVar rigidVariables -> Nothing
          | otherwise -> bindTypeVar leftVar resolvedRight stateAfterDereference
        (_, TVarType rightVar)
          | Set.member rightVar rigidVariables -> Nothing
          | otherwise -> bindTypeVar rightVar resolvedLeft stateAfterDereference
        _ -> Nothing
  where
    rigidVariables = inferRigidTypeVars state

-- Unification needs only the outer constructor at each recursive step. Follow
-- variable chains here and retain their shorter equivalent in the solver;
-- compound children are visited exactly where their pair is unified.
dereferenceType :: InferState -> ExpressionType -> (ExpressionType, InferState)
dereferenceType state expressionType =
  case expressionType of
    TVarType typeVar ->
      case Map.lookup typeVar (inferSubst state) of
        Nothing -> (expressionType, state)
        Just replacementType@(TVarType replacementVar) ->
          let (resolvedType, resolvedState) = dereferenceType state replacementType
              compressedState =
                case resolvedType of
                  TVarType resolvedVar
                    | resolvedVar == replacementVar -> resolvedState
                  _ ->
                    modifySolverState
                      ( \solver ->
                          solver
                            { solverSubstitution =
                                Map.insert typeVar resolvedType (solverSubstitution solver)
                            }
                      )
                      resolvedState
           in (resolvedType, compressedState)
        Just replacementType ->
          (replacementType, state)
    _ -> (expressionType, state)

unifyTypeLists :: [ExpressionType] -> [ExpressionType] -> InferState -> Maybe InferState
unifyTypeLists leftTypes rightTypes state =
  {-# SCC "jazz-stage:constraint-solving" #-}
  unifyTypeListsWithoutCostCentre leftTypes rightTypes state

unifyTypeListsWithoutCostCentre :: [ExpressionType] -> [ExpressionType] -> InferState -> Maybe InferState
unifyTypeListsWithoutCostCentre leftTypes rightTypes state
  | length leftTypes /= length rightTypes = Nothing
  | otherwise = foldl' step (Just state) (zip leftTypes rightTypes)
  where
    step maybeState (leftType, rightType) =
      maybeState >>= unifyTypesWithoutCostCentre leftType rightType

bindTypeVar :: InferenceVariable -> ExpressionType -> InferState -> Maybe InferState
bindTypeVar typeVar replacementType state
  | resolvedReplacementType == TVarType typeVar = Just state
  | occursInType typeVar resolvedReplacementType = Nothing
  | literalConstraintWouldSpecializeRigidVariable = Nothing
  | typeVarIsStrictEqualityConstrained
      && not (supportsDeferredEqualityOperandType state resolvedReplacementType) =
      Nothing
  | otherwise = do
      nextReplacementType <- constrainedReplacementType
      pure
        ( modifySolverState
            ( \solver ->
                solver
                  { solverSubstitution =
                      Map.insert typeVar nextReplacementType (solverSubstitution solver),
                    solverStrictEqualityVars =
                      nextStrictEqualityVars nextReplacementType
                  }
            )
            (stateAfterNumericConstraint nextReplacementType)
        )
  where
    resolvedReplacementType = resolveType state replacementType
    typeVarIsStrictEqualityConstrained =
      Set.member typeVar (inferStrictEqualityVars state)
    typeVarNumericConstraint = Map.lookup typeVar (inferNumericVars state)
    literalConstraintWouldSpecializeRigidVariable =
      case (typeVarNumericConstraint, resolvedReplacementType) of
        (Just IntegralLiteralNumericConstraint {}, TVarType replacementVar) ->
          Set.member replacementVar (inferRigidTypeVars state)
        _ -> False
    constrainedReplacementType =
      case typeVarNumericConstraint of
        Just numericConstraint ->
          applyNumericConstraintToReplacement numericConstraint resolvedReplacementType
        Nothing -> Just resolvedReplacementType
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

occursInType :: InferenceVariable -> ExpressionType -> Bool
occursInType typeVar expressionType =
  case expressionType of
    TIntType -> False
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

addStrictEqualityTypeVarConstraint :: InferenceVariable -> InferState -> InferState
addStrictEqualityTypeVarConstraint typeVar state =
  modifySolverState
    ( \solver ->
        solver
          { solverStrictEqualityVars =
              Set.insert typeVar (solverStrictEqualityVars solver)
          }
    )
    state

addNumericTypeVarConstraint :: InferenceVariable -> NumericConstraint -> InferState -> InferState
addNumericTypeVarConstraint typeVar numericConstraint state =
  modifySolverState
    ( \solver ->
        solver
          { solverNumericVars =
              Map.insertWith
                combineNumericConstraints
                typeVar
                numericConstraint
                (solverNumericVars solver)
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
applyNumericConstraintToReplacement numericConstraint replacementType
  | typeSatisfiesNumericConstraint numericConstraint replacementType = Just replacementType
  | otherwise = Nothing

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
        TNumericType numericType ->
          numericTypeIsIntegral numericType
            && integerLiteralRangeFitsNumericType literalRange numericType
        TVarType {} -> True
        _ -> False
  where
    anyNumeric =
      case expressionType of
        TIntType -> True
        TFloatType -> True
        TNumericType {} -> True
        TVarType {} -> True
        _ -> False
    runtimeArithmeticNumeric =
      case expressionType of
        TIntType -> True
        TFloatType -> True
        TNumericType numericType -> numericTypeSupportsRuntimeArithmetic numericType
        TVarType {} -> True
        _ -> False
    runtimeComparisonNumeric =
      case expressionType of
        TIntType -> True
        TFloatType -> True
        TNumericType numericType -> numericTypeSupportsRuntimeComparison numericType
        TVarType {} -> True
        _ -> False
    integralNumeric =
      case expressionType of
        TIntType -> True
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
supportsRuntimeEqualityTypeWith seenDataTypes state expressionType
  | Just _ <- integerLiteralRangeFor state expressionType = True
  | otherwise =
      case resolveType state expressionType of
        TIntType -> True
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
   in if Set.member dataTypeKey seenDataTypes
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
        ConstructorArgumentStructured fieldType ->
          maybe
            False
            (supportsRuntimeEqualityTypeWith nextSeenDataTypes state)
            (instantiateConstructorFieldType typeParameterBindings fieldType)
        ConstructorArgumentFresh -> False

supportsDeferredEqualityOperandType :: InferState -> ExpressionType -> Bool
supportsDeferredEqualityOperandType state expressionType =
  case resolveType state expressionType of
    TVarType _ -> True
    _ -> supportsRuntimeEqualityType state expressionType

modifySolverState :: (SolverState -> SolverState) -> InferState -> InferState
modifySolverState update state =
  state {inferSolver = update (inferSolver state)}
