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
    freshTypeVars,
    freshTypeVariable,
    freshIntegerLiteralType,
    integerLiteralRangeFor,
    integerLiteralRangeBounds,
    integerLiteralRangeFitsNumericType,
    occursInType,
    resolveType,
    supportsRuntimeEqualityType,
    typeSatisfiesNumericConstraint,
    unifyTypes,
  )
where

import Control.Monad (replicateM)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.BuiltinCatalog
  ( numericTypeIntegerBounds,
    numericTypeIsIntegral,
  )
import Jazz.Compiler.Name (ResolvedName, identifierText)
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
    ExpressionType,
    InferenceVariable,
    IntegerLiteralRange (..),
    NumericConstraint (..),
    SemanticType (..),
    instantiateDeclarationType,
  )
import Jazz.Compiler.TypeRepresentation (NumericType (..), substituteSemanticVariables)

freshTypeVar :: InferState -> (ExpressionType, InferState)
freshTypeVar state =
  let (_, expressionType, nextState) = freshTypeVariable state
   in (expressionType, nextState)

freshTypeVars :: Int -> InferState -> ([ExpressionType], InferState)
freshTypeVars count = State.runState (replicateM count (State.state freshTypeVar))

freshTypeVariable :: InferState -> (InferenceVariable, ExpressionType, InferState)
freshTypeVariable state =
  let nextVar = inferNextTypeVar state
   in ( nextVar,
        SemanticVariable nextVar,
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
    SemanticVariable typeVar -> do
      IntegralLiteralNumericConstraint literalRange <- Map.lookup typeVar (inferNumericVars state)
      pure literalRange
    _ -> Nothing

resolveType :: InferState -> ExpressionType -> ExpressionType
resolveType state = applySubstitution (inferSubst state)

applySubstitution :: Map.Map InferenceVariable ExpressionType -> ExpressionType -> ExpressionType
applySubstitution substitution = resolve
  where
    resolve = substituteSemanticVariables replace
    replace variable =
      maybe (SemanticVariable variable) resolve (Map.lookup variable substitution)

unifyTypes :: ExpressionType -> ExpressionType -> InferState -> Maybe InferState
unifyTypes leftType rightType state =
  {-# SCC "jazz-stage:constraint-solving" #-}
  unifyTypesWithoutCostCentre leftType rightType state

unifyTypesWithoutCostCentre :: ExpressionType -> ExpressionType -> InferState -> Maybe InferState
unifyTypesWithoutCostCentre leftType rightType state =
  let (resolvedLeft, stateAfterLeft) = dereferenceType state leftType
      (resolvedRight, stateAfterDereference) = dereferenceType stateAfterLeft rightType
   in case (resolvedLeft, resolvedRight) of
        (SemanticInt, SemanticInt) -> Just stateAfterDereference
        (SemanticFloat, SemanticFloat) -> Just stateAfterDereference
        (SemanticFloat, SemanticNumeric NumericFloat64) -> Just stateAfterDereference
        (SemanticNumeric NumericFloat64, SemanticFloat) -> Just stateAfterDereference
        (SemanticInt, SemanticNumeric NumericInt64) -> Just stateAfterDereference
        (SemanticNumeric NumericInt64, SemanticInt) -> Just stateAfterDereference
        (SemanticNumeric leftNumericType, SemanticNumeric rightNumericType)
          | leftNumericType == rightNumericType -> Just stateAfterDereference
        (SemanticBool, SemanticBool) -> Just stateAfterDereference
        (SemanticChar, SemanticChar) -> Just stateAfterDereference
        (SemanticText, SemanticText) -> Just stateAfterDereference
        (SemanticData leftName leftArguments, SemanticData rightName rightArguments)
          | leftName == rightName ->
              unifyTypeListsWithoutCostCentre leftArguments rightArguments stateAfterDereference
        (SemanticList leftElementType, SemanticList rightElementType) ->
          unifyTypesWithoutCostCentre leftElementType rightElementType stateAfterDereference
        (SemanticTuple leftElementTypes, SemanticTuple rightElementTypes) ->
          unifyTypeListsWithoutCostCentre leftElementTypes rightElementTypes stateAfterDereference
        ( SemanticFunction leftInputType leftOutputType,
          SemanticFunction rightInputType rightOutputType
          ) -> do
            stateAfterInput <- unifyTypesWithoutCostCentre leftInputType rightInputType stateAfterDereference
            unifyTypesWithoutCostCentre leftOutputType rightOutputType stateAfterInput
        (SemanticVariable leftVar, SemanticVariable rightVar)
          | leftVar == rightVar ->
              Just stateAfterDereference
          | Set.member leftVar rigidVariables,
            Set.member rightVar rigidVariables ->
              Nothing
          | Set.member leftVar rigidVariables ->
              bindTypeVar rightVar resolvedLeft stateAfterDereference
          | Set.member rightVar rigidVariables ->
              bindTypeVar leftVar resolvedRight stateAfterDereference
        (SemanticVariable leftVar, _)
          | Set.member leftVar rigidVariables -> Nothing
          | otherwise -> bindTypeVar leftVar resolvedRight stateAfterDereference
        (_, SemanticVariable rightVar)
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
    SemanticVariable typeVar ->
      case Map.lookup typeVar (inferSubst state) of
        Nothing -> (expressionType, state)
        Just replacementType@(SemanticVariable replacementVar) ->
          let (resolvedType, resolvedState) = dereferenceType state replacementType
              compressedState =
                case resolvedType of
                  SemanticVariable resolvedVar
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

unifyTypeListsWithoutCostCentre :: [ExpressionType] -> [ExpressionType] -> InferState -> Maybe InferState
unifyTypeListsWithoutCostCentre leftTypes rightTypes state
  | length leftTypes /= length rightTypes = Nothing
  | otherwise = foldl' step (Just state) (zip leftTypes rightTypes)
  where
    step maybeState (leftType, rightType) =
      maybeState >>= unifyTypesWithoutCostCentre leftType rightType

bindTypeVar :: InferenceVariable -> ExpressionType -> InferState -> Maybe InferState
bindTypeVar typeVar replacementType state
  | resolvedReplacementType == SemanticVariable typeVar = Just state
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
        (Just IntegralLiteralNumericConstraint {}, SemanticVariable replacementVar) ->
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
        SemanticVariable replacementVar
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
        (Just numericConstraint, SemanticVariable replacementVar) ->
          addNumericTypeVarConstraint replacementVar numericConstraint stateWithoutNumericTypeVar
        _ -> stateWithoutNumericTypeVar

occursInType :: InferenceVariable -> ExpressionType -> Bool
occursInType typeVar expressionType =
  case expressionType of
    SemanticInt -> False
    SemanticFloat -> False
    SemanticNumeric {} -> False
    SemanticBool -> False
    SemanticChar -> False
    SemanticText -> False
    SemanticList elementType -> occursInType typeVar elementType
    SemanticTuple elementTypes -> any (occursInType typeVar) elementTypes
    SemanticData _ typeArguments -> any (occursInType typeVar) typeArguments
    SemanticFunction inputType outputType ->
      occursInType typeVar inputType || occursInType typeVar outputType
    SemanticVariable otherVar -> typeVar == otherVar

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
    SemanticVariable typeVar -> Just (addNumericTypeVarConstraint typeVar numericConstraint state)
    resolvedType
      | typeSatisfiesNumericConstraint numericConstraint resolvedType -> Just state
      | otherwise -> Nothing

typeSatisfiesNumericConstraint :: NumericConstraint -> ExpressionType -> Bool
typeSatisfiesNumericConstraint numericConstraint expressionType =
  case numericConstraint of
    AnyNumericConstraint -> anyNumeric
    RuntimeArithmeticNumericConstraint -> anyNumeric
    RuntimeComparisonNumericConstraint -> anyNumeric
    IntegralNumericConstraint -> integralNumeric
    IntegralLiteralNumericConstraint literalRange ->
      case expressionType of
        SemanticInt -> True
        SemanticNumeric numericType ->
          numericTypeIsIntegral numericType
            && integerLiteralRangeFitsNumericType literalRange numericType
        SemanticVariable {} -> True
        _ -> False
  where
    anyNumeric =
      case expressionType of
        SemanticInt -> True
        SemanticFloat -> True
        SemanticNumeric {} -> True
        SemanticVariable {} -> True
        _ -> False
    integralNumeric =
      case expressionType of
        SemanticInt -> True
        SemanticNumeric numericType -> numericTypeIsIntegral numericType
        SemanticVariable {} -> True
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

supportsRuntimeEqualityTypeWith :: Set.Set (Text, [ExpressionType]) -> InferState -> ExpressionType -> Bool
supportsRuntimeEqualityTypeWith seenDataTypes state expressionType
  | Just _ <- integerLiteralRangeFor state expressionType = True
  | otherwise =
      case resolveType state expressionType of
        SemanticInt -> True
        SemanticFloat -> True
        SemanticNumeric {} -> True
        SemanticBool -> True
        SemanticChar -> True
        SemanticText -> True
        SemanticList elementType -> supportsRuntimeEqualityTypeWith seenDataTypes state elementType
        SemanticTuple elementTypes -> all (supportsRuntimeEqualityTypeWith seenDataTypes state) elementTypes
        SemanticData typeName typeArguments ->
          dataTypeSupportsRuntimeEqualityWith seenDataTypes state typeName typeArguments
        _ -> False

dataTypeSupportsRuntimeEqualityWith :: Set.Set (Text, [ExpressionType]) -> InferState -> ResolvedName -> [ExpressionType] -> Bool
dataTypeSupportsRuntimeEqualityWith seenDataTypes state typeName typeArguments =
  let resolvedTypeArguments = map (resolveType state) typeArguments
      dataTypeKey = (identifierText typeName, resolvedTypeArguments)
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
        ConstructorArgumentType fieldType ->
          maybe
            False
            (supportsRuntimeEqualityTypeWith nextSeenDataTypes state)
            (instantiateDeclarationType typeParameterBindings fieldType)
        ConstructorArgumentFresh -> False

supportsDeferredEqualityOperandType :: InferState -> ExpressionType -> Bool
supportsDeferredEqualityOperandType state expressionType =
  case resolveType state expressionType of
    SemanticVariable _ -> True
    _ -> supportsRuntimeEqualityType state expressionType

modifySolverState :: (SolverState -> SolverState) -> InferState -> InferState
modifySolverState update state =
  state {inferSolver = update (inferSolver state)}
