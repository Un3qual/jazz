{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpression,
    inferExpressionDefault
  ) where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Analyzer
  ( AnalysisResult (..),
    analyzeProgram
  )
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    lookupBuiltinSymbol
  )
import JazzNext.Compiler.Diagnostics
  ( WarningRecord
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    defaultWarningSettings
  )

data InferenceResult = InferenceResult
  { inferredExpr :: Expr,
    inferredWarnings :: [WarningRecord],
    inferredErrors :: [Text]
  }
  deriving (Eq, Show)

-- This currently forwards analyzer diagnostics while the richer inference/type
-- pipeline is still being built in jazz-next.
inferExpression :: WarningSettings -> Expr -> IO InferenceResult
inferExpression settings expr = do
  let canonicalExpr = canonicalizeExpr expr
  AnalysisResult _ warnings errors <- analyzeProgram settings canonicalExpr
  let typeErrors = collectExprTypeErrors canonicalExpr
  pure
    InferenceResult
      { inferredExpr = canonicalExpr,
        inferredWarnings = warnings,
        inferredErrors = errors ++ typeErrors
      }

inferExpressionDefault :: Expr -> IO InferenceResult
inferExpressionDefault = inferExpression defaultWarningSettings

-- Keep if/case canonicalization local so new AST variants do not depend on the
-- legacy desugar module shape.
canonicalizeExpr :: Expr -> Expr
canonicalizeExpr expr =
  case expr of
    EInt value -> EInt value
    EBool value -> EBool value
    EVar name -> EVar name
    EList elements -> EList (map canonicalizeExpr elements)
    EApply functionExpr argumentExpr ->
      EApply (canonicalizeExpr functionExpr) (canonicalizeExpr argumentExpr)
    EIf conditionExpr thenExpr elseExpr ->
      ECase
        (canonicalizeExpr conditionExpr)
        (canonicalizeExpr thenExpr)
        (canonicalizeExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr ->
      ECase
        (canonicalizeExpr conditionExpr)
        (canonicalizeExpr thenExpr)
        (canonicalizeExpr elseExpr)
    EBinary operatorSymbol leftExpr rightExpr ->
      EBinary
        operatorSymbol
        (canonicalizeExpr leftExpr)
        (canonicalizeExpr rightExpr)
    ESectionLeft leftExpr operatorSymbol ->
      ESectionLeft (canonicalizeExpr leftExpr) operatorSymbol
    ESectionRight operatorSymbol rightExpr ->
      ESectionRight operatorSymbol (canonicalizeExpr rightExpr)
    EScope statements -> EScope (map canonicalizeStatement statements)

canonicalizeStatement :: Statement -> Statement
canonicalizeStatement statement =
  case statement of
    SLet name spanValue valueExpr ->
      SLet name spanValue (canonicalizeExpr valueExpr)
    SSignature name spanValue signatureText ->
      SSignature name spanValue signatureText
    SExpr spanValue expr ->
      SExpr spanValue (canonicalizeExpr expr)

data ExpressionType
  = TIntType
  | TBoolType
  | TListType ExpressionType
  | TFunctionType ExpressionType ExpressionType
  | TVarType Int
  deriving (Eq, Show)

data InferState = InferState
  { inferNextTypeVar :: Int,
    inferSubst :: Map Int ExpressionType,
    -- Type variables originating from strict-equality sections must eventually
    -- resolve to runtime-supported equality families.
    inferStrictEqualityVars :: Set Int,
    inferErrorsRev :: [Text]
  }

initialInferState :: InferState
initialInferState =
  InferState
    { inferNextTypeVar = 0,
      inferSubst = Map.empty,
      inferStrictEqualityVars = Set.empty,
      inferErrorsRev = []
    }

collectExprTypeErrors :: Expr -> [Text]
collectExprTypeErrors expr =
  let (_, finalState) = inferExprType Map.empty initialInferState expr
   in reverse (inferErrorsRev finalState)

inferExprType :: Map Text ExpressionType -> InferState -> Expr -> (Maybe ExpressionType, InferState)
inferExprType env state expr =
  case expr of
    EInt _ -> (Just TIntType, state)
    EBool _ -> (Just TBoolType, state)
    EVar name ->
      case Map.lookup name env of
        Just localType -> (Just (resolveType state localType), state)
        Nothing ->
          case instantiateBuiltinType name state of
            Just (builtinType, nextState) -> (Just builtinType, nextState)
            Nothing -> (Nothing, state)
    EList elements -> inferListType env state elements
    EApply functionExpr argumentExpr ->
      let (functionType, stateAfterFunction) = inferExprType env state functionExpr
          (argumentType, stateAfterArgument) = inferExprType env stateAfterFunction argumentExpr
          (resultTypeVar, stateWithResultVar) = freshTypeVar stateAfterArgument
       in case (functionType, argumentType) of
            (Just inferredFunctionType, Just inferredArgumentType) ->
              case
                  unifyTypes
                    inferredFunctionType
                    (TFunctionType inferredArgumentType resultTypeVar)
                    stateWithResultVar of
                Just unifiedState ->
                  (Just (resolveType unifiedState resultTypeVar), unifiedState)
                Nothing ->
                  ( Nothing,
                    addTypeError
                      stateWithResultVar
                      ( mkApplyTypeError
                          (resolveType stateWithResultVar inferredFunctionType)
                          (resolveType stateWithResultVar inferredArgumentType)
                      )
                  )
            _ -> (Nothing, stateWithResultVar)
    EIf conditionExpr thenExpr elseExpr ->
      inferExprType env state (ECase conditionExpr thenExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr ->
      let (conditionType, stateAfterCondition) = inferExprType env state conditionExpr
          (thenType, stateAfterThen) = inferExprType env stateAfterCondition thenExpr
          (elseType, stateAfterElse) = inferExprType env stateAfterThen elseExpr
          stateAfterConditionCheck =
            case conditionType of
              Just inferredConditionType ->
                case unifyTypes inferredConditionType TBoolType stateAfterElse of
                  Just unifiedState -> unifiedState
                  Nothing ->
                    addTypeError
                      stateAfterElse
                      (mkIfConditionTypeError (resolveType stateAfterElse inferredConditionType))
              Nothing -> stateAfterElse
       in
        case (thenType, elseType) of
          (Just inferredThenType, Just inferredElseType) ->
            case unifyTypes inferredThenType inferredElseType stateAfterConditionCheck of
              Just unifiedState ->
                (Just (resolveType unifiedState inferredThenType), unifiedState)
              Nothing ->
                ( Nothing,
                  addTypeError
                    stateAfterConditionCheck
                    ( mkIfBranchTypeMismatchError
                        (resolveType stateAfterConditionCheck inferredThenType)
                        (resolveType stateAfterConditionCheck inferredElseType)
                    )
                )
          _ -> (Nothing, stateAfterConditionCheck)
    EBinary operatorSymbol leftExpr rightExpr ->
      let (leftType, stateAfterLeft) = inferExprType env state leftExpr
          (rightType, stateAfterRight) = inferExprType env stateAfterLeft rightExpr
       in case (leftType, rightType) of
            (Just inferredLeftType, Just inferredRightType) ->
              inferBinaryType operatorSymbol inferredLeftType inferredRightType stateAfterRight
            _ -> (Nothing, stateAfterRight)
    ESectionLeft leftExpr operatorSymbol ->
      let (leftType, stateAfterLeft) = inferExprType env state leftExpr
       in case leftType of
            Just inferredLeftType ->
              inferSectionLeftType operatorSymbol inferredLeftType stateAfterLeft
            Nothing -> (Nothing, stateAfterLeft)
    ESectionRight operatorSymbol rightExpr ->
      let (rightType, stateAfterRight) = inferExprType env state rightExpr
       in case rightType of
            Just inferredRightType ->
              inferSectionRightType operatorSymbol inferredRightType stateAfterRight
            Nothing -> (Nothing, stateAfterRight)
    EScope statements -> inferScopeType env state statements

inferListType :: Map Text ExpressionType -> InferState -> [Expr] -> (Maybe ExpressionType, InferState)
inferListType env state elements =
  case elements of
    [] ->
      let (elementType, nextState) = freshTypeVar state
       in (Just (TListType elementType), nextState)
    firstElement : restElements ->
      let (firstType, stateAfterFirst) = inferExprType env state firstElement
          (finalElementType, finalState) =
            foldl
              step
              (firstType, stateAfterFirst)
              restElements
       in (TListType <$> finalElementType, finalState)
  where
    step :: (Maybe ExpressionType, InferState) -> Expr -> (Maybe ExpressionType, InferState)
    step (expectedType, stateAcc) element =
      let (actualType, stateAfterElement) = inferExprType env stateAcc element
       in case (expectedType, actualType) of
            (Just inferredExpectedType, Just inferredActualType) ->
              case unifyTypes inferredExpectedType inferredActualType stateAfterElement of
                Just unifiedState ->
                  (Just (resolveType unifiedState inferredExpectedType), unifiedState)
                Nothing ->
                  ( Just inferredExpectedType,
                    addTypeError
                      stateAfterElement
                      ( mkListElementTypeMismatchError
                          (resolveType stateAfterElement inferredExpectedType)
                          (resolveType stateAfterElement inferredActualType)
                      )
                  )
            _ -> (expectedType, stateAfterElement)

data OperatorRule
  = NumericRule ExpressionType
  | StrictEqualityRule

lookupOperatorRule :: Text -> Maybe OperatorRule
lookupOperatorRule operatorSymbol =
  case operatorSymbol of
    "+" -> Just (NumericRule TIntType)
    "-" -> Just (NumericRule TIntType)
    "*" -> Just (NumericRule TIntType)
    "/" -> Just (NumericRule TIntType)
    "<" -> Just (NumericRule TBoolType)
    "<=" -> Just (NumericRule TBoolType)
    ">" -> Just (NumericRule TBoolType)
    ">=" -> Just (NumericRule TBoolType)
    "==" -> Just StrictEqualityRule
    "!=" -> Just StrictEqualityRule
    _ -> Nothing

inferBinaryType :: Text -> ExpressionType -> ExpressionType -> InferState -> (Maybe ExpressionType, InferState)
inferBinaryType operatorSymbol leftType rightType state =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule resultType) ->
      applyNumericBinaryRule operatorSymbol resultType leftType rightType state
    Just StrictEqualityRule ->
      applyStrictEqualityBinaryRule operatorSymbol leftType rightType state
    Nothing ->
      ( Nothing,
        addTypeError
          state
          ( mkBinaryTypeError
              operatorSymbol
              (resolveType state leftType)
              (resolveType state rightType)
          )
      )

applyNumericBinaryRule ::
  Text ->
  ExpressionType ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyNumericBinaryRule operatorSymbol resultType leftType rightType state =
  case unifyTypes leftType TIntType state of
    Just stateAfterLeft ->
      case unifyTypes rightType TIntType stateAfterLeft of
        Just stateAfterRight -> (Just resultType, stateAfterRight)
        Nothing -> numericOperandError stateAfterLeft
    Nothing -> numericOperandError state
  where
    numericOperandError errState =
      ( Nothing,
        addTypeError
          errState
          ( mkBinaryTypeError
              operatorSymbol
              (resolveType errState leftType)
              (resolveType errState rightType)
          )
      )

applyStrictEqualityBinaryRule ::
  Text ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyStrictEqualityBinaryRule operatorSymbol leftType rightType state =
  case unifyTypes leftType rightType state of
    Just unifiedState ->
      let resolvedType = resolveType unifiedState leftType
       in
        if supportsRuntimeEqualityType resolvedType
          then (Just TBoolType, unifiedState)
          else
            ( Nothing,
              addTypeError
                unifiedState
                (mkStrictEqualityUnsupportedTypeError operatorSymbol resolvedType)
            )
    Nothing ->
      ( Nothing,
        addTypeError
          state
          ( mkStrictEqualityTypeError
              operatorSymbol
              (resolveType state leftType)
              (resolveType state rightType)
          )
      )

inferSectionLeftType ::
  Text ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
inferSectionLeftType operatorSymbol leftType state =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule resultType) ->
      applyNumericSectionLeftRule operatorSymbol resultType leftType state
    Just StrictEqualityRule ->
      applyStrictEqualitySectionLeftRule operatorSymbol leftType state
    _ ->
      ( Nothing,
        addTypeError
          state
          (mkUnsupportedSectionOperatorError operatorSymbol)
      )

applyNumericSectionLeftRule ::
  Text ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyNumericSectionLeftRule operatorSymbol resultType leftType state =
  case unifyTypes leftType TIntType state of
    Just unifiedState ->
      (Just (TFunctionType TIntType resultType), unifiedState)
    Nothing ->
      ( Nothing,
        addTypeError
          state
          ( mkBinaryTypeError
              operatorSymbol
              (resolveType state leftType)
              TIntType
          )
      )

applyStrictEqualitySectionLeftRule ::
  Text ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyStrictEqualitySectionLeftRule operatorSymbol leftType state =
  let resolvedLeftType = resolveType state leftType
   in
    case resolvedLeftType of
      TVarType typeVar ->
        ( Just (TFunctionType resolvedLeftType TBoolType),
          addStrictEqualityTypeVarConstraint typeVar state
        )
      _
        | supportsRuntimeEqualityType resolvedLeftType ->
            (Just (TFunctionType resolvedLeftType TBoolType), state)
        | otherwise ->
            ( Nothing,
              addTypeError
                state
                (mkStrictEqualityUnsupportedTypeError operatorSymbol resolvedLeftType)
            )

inferSectionRightType ::
  Text ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
inferSectionRightType operatorSymbol rightType state =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule resultType) ->
      applyNumericSectionRightRule operatorSymbol resultType rightType state
    Just StrictEqualityRule ->
      applyStrictEqualitySectionRightRule operatorSymbol rightType state
    _ ->
      ( Nothing,
        addTypeError
          state
          (mkUnsupportedSectionOperatorError operatorSymbol)
      )

applyNumericSectionRightRule ::
  Text ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyNumericSectionRightRule operatorSymbol resultType rightType state =
  case unifyTypes rightType TIntType state of
    Just unifiedState ->
      (Just (TFunctionType TIntType resultType), unifiedState)
    Nothing ->
      ( Nothing,
        addTypeError
          state
          ( mkBinaryTypeError
              operatorSymbol
              TIntType
              (resolveType state rightType)
          )
      )

applyStrictEqualitySectionRightRule ::
  Text ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyStrictEqualitySectionRightRule operatorSymbol rightType state =
  let resolvedRightType = resolveType state rightType
   in
    case resolvedRightType of
      TVarType typeVar ->
        ( Just (TFunctionType resolvedRightType TBoolType),
          addStrictEqualityTypeVarConstraint typeVar state
        )
      _
        | supportsRuntimeEqualityType resolvedRightType ->
            (Just (TFunctionType resolvedRightType TBoolType), state)
        | otherwise ->
            ( Nothing,
              addTypeError
                state
                (mkStrictEqualityUnsupportedTypeError operatorSymbol resolvedRightType)
            )

inferScopeType :: Map Text ExpressionType -> InferState -> [Statement] -> (Maybe ExpressionType, InferState)
inferScopeType initialEnv initialState statements = go initialEnv Nothing Nothing initialState statements
  where
    go env lastExprType pendingSignatureType state remainingStatements =
      case remainingStatements of
        [] -> (lastExprType, state)
        statement : rest ->
          case statement of
            SSignature name _ signatureText ->
              let (nextPendingSignature, nextState) =
                    case parseSignatureType signatureText of
                      Just signatureType ->
                        (Just (PendingSignatureType name signatureType), state)
                      Nothing ->
                        ( Nothing,
                          addTypeError
                            state
                            (mkInvalidSignatureTypeError name signatureText)
                        )
               in go env lastExprType nextPendingSignature nextState rest
            SLet name _ valueExpr ->
              let envWithPendingSignature =
                    case pendingSignatureType of
                      Just pendingSignature
                        | pendingSignatureName pendingSignature == name ->
                            Map.insert
                              name
                              (pendingSignatureDeclaredType pendingSignature)
                              env
                      _ -> env
                  (valueType, stateAfterValue) = inferExprType envWithPendingSignature state valueExpr
                  stateAfterSignatureCheck =
                    case (pendingSignatureType, valueType) of
                      (Just pendingSignature, Just inferredType)
                        | pendingSignatureName pendingSignature == name ->
                            case
                                unifyTypes
                                  (pendingSignatureDeclaredType pendingSignature)
                                  inferredType
                                  stateAfterValue of
                              Just unifiedState -> unifiedState
                              Nothing ->
                                addTypeError
                                  stateAfterValue
                                  ( mkSignatureTypeMismatchError
                                      name
                                      (resolveType stateAfterValue (pendingSignatureDeclaredType pendingSignature))
                                      (resolveType stateAfterValue inferredType)
                                  )
                      _ -> stateAfterValue
                  nextBindingType =
                    case pendingSignatureType of
                      Just pendingSignature
                        | pendingSignatureName pendingSignature == name ->
                            Just (resolveType stateAfterSignatureCheck (pendingSignatureDeclaredType pendingSignature))
                      _ -> fmap (resolveType stateAfterSignatureCheck) valueType
                  nextEnv =
                    case nextBindingType of
                      Just inferredType -> Map.insert name inferredType env
                      Nothing -> env
               in go nextEnv lastExprType Nothing stateAfterSignatureCheck rest
            SExpr _ expr ->
              let (exprType, stateAfterExpr) = inferExprType env state expr
               in go env exprType Nothing stateAfterExpr rest

data PendingSignatureType = PendingSignatureType
  { pendingSignatureName :: Text,
    pendingSignatureDeclaredType :: ExpressionType
  }

parseSignatureType :: Text -> Maybe ExpressionType
parseSignatureType signatureText =
  case Text.filter (not . isSpace) signatureText of
    "Int" -> Just TIntType
    "Bool" -> Just TBoolType
    _ -> Nothing

instantiateBuiltinType :: Text -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinType name state =
  case lookupBuiltinSymbol name of
    Just BuiltinHd ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (TFunctionType (TListType elementType) elementType, stateAfterElement)
    Just BuiltinTl ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (TFunctionType (TListType elementType) (TListType elementType), stateAfterElement)
    Just BuiltinMap ->
      let (sourceType, stateAfterSource) = freshTypeVar state
          (targetType, stateAfterTarget) = freshTypeVar stateAfterSource
       in
        Just
          ( TFunctionType
              (TFunctionType sourceType targetType)
              (TFunctionType (TListType sourceType) (TListType targetType)),
            stateAfterTarget
          )
    Nothing -> Nothing

freshTypeVar :: InferState -> (ExpressionType, InferState)
freshTypeVar state =
  let nextVar = inferNextTypeVar state
   in (TVarType nextVar, state {inferNextTypeVar = nextVar + 1})

resolveType :: InferState -> ExpressionType -> ExpressionType
resolveType state = applySubstitution (inferSubst state)

applySubstitution :: Map Int ExpressionType -> ExpressionType -> ExpressionType
applySubstitution subst expressionType =
  case expressionType of
    TIntType -> TIntType
    TBoolType -> TBoolType
    TListType elementType -> TListType (applySubstitution subst elementType)
    TFunctionType inputType outputType ->
      TFunctionType
        (applySubstitution subst inputType)
        (applySubstitution subst outputType)
    TVarType typeVar ->
      case Map.lookup typeVar subst of
        Just replacementType -> applySubstitution subst replacementType
        Nothing -> TVarType typeVar

unifyTypes :: ExpressionType -> ExpressionType -> InferState -> Maybe InferState
unifyTypes leftType rightType state =
  let resolvedLeft = resolveType state leftType
      resolvedRight = resolveType state rightType
   in case (resolvedLeft, resolvedRight) of
        (TIntType, TIntType) -> Just state
        (TBoolType, TBoolType) -> Just state
        (TListType leftElementType, TListType rightElementType) ->
          unifyTypes leftElementType rightElementType state
        ( TFunctionType leftInputType leftOutputType,
          TFunctionType rightInputType rightOutputType
          ) -> do
          stateAfterInput <- unifyTypes leftInputType rightInputType state
          unifyTypes leftOutputType rightOutputType stateAfterInput
        (TVarType leftVar, _) -> bindTypeVar leftVar resolvedRight state
        (_, TVarType rightVar) -> bindTypeVar rightVar resolvedLeft state
        _ -> Nothing

bindTypeVar :: Int -> ExpressionType -> InferState -> Maybe InferState
bindTypeVar typeVar replacementType state
  | replacementType == TVarType typeVar = Just state
  | occursInType typeVar replacementType = Nothing
  -- Preserve compile/runtime contract when deferred section vars later unify.
  | typeVarIsStrictEqualityConstrained && not (supportsDeferredEqualityOperandType replacementType) =
      Nothing
  | otherwise =
      Just
        state
          { inferSubst = Map.insert typeVar replacementType (inferSubst state),
            inferStrictEqualityVars = nextStrictEqualityVars
          }
  where
    typeVarIsStrictEqualityConstrained =
      Set.member typeVar (inferStrictEqualityVars state)
    strictEqualityVarsWithoutTypeVar =
      Set.delete typeVar (inferStrictEqualityVars state)
    nextStrictEqualityVars =
      case replacementType of
        TVarType replacementVar
          | typeVarIsStrictEqualityConstrained ->
              Set.insert replacementVar strictEqualityVarsWithoutTypeVar
        _ -> strictEqualityVarsWithoutTypeVar

occursInType :: Int -> ExpressionType -> Bool
occursInType typeVar expressionType =
  case expressionType of
    TIntType -> False
    TBoolType -> False
    TListType elementType -> occursInType typeVar elementType
    TFunctionType inputType outputType ->
      occursInType typeVar inputType || occursInType typeVar outputType
    TVarType otherVar -> typeVar == otherVar

addTypeError :: InferState -> Text -> InferState
addTypeError state errorText =
  state {inferErrorsRev = errorText : inferErrorsRev state}

addStrictEqualityTypeVarConstraint :: Int -> InferState -> InferState
addStrictEqualityTypeVarConstraint typeVar state =
  state
    { inferStrictEqualityVars =
        Set.insert typeVar (inferStrictEqualityVars state)
    }

mkBinaryTypeError :: Text -> ExpressionType -> ExpressionType -> Text
mkBinaryTypeError operatorSymbol leftType rightType =
  "E2003: cannot apply operator '"
    <> operatorSymbol
    <> "' to operands of type "
    <> renderType leftType
    <> " and "
    <> renderType rightType

mkStrictEqualityTypeError :: Text -> ExpressionType -> ExpressionType -> Text
mkStrictEqualityTypeError operatorSymbol leftType rightType =
  "E2004: strict equality operator '"
    <> operatorSymbol
    <> "' requires operands of the same type, found "
    <> renderType leftType
    <> " and "
    <> renderType rightType

mkStrictEqualityUnsupportedTypeError :: Text -> ExpressionType -> Text
mkStrictEqualityUnsupportedTypeError operatorSymbol foundType =
  "E2004: strict equality operator '"
    <> operatorSymbol
    <> "' is only supported for Int and Bool operands, found "
    <> renderType foundType

mkSignatureTypeMismatchError :: Text -> ExpressionType -> ExpressionType -> Text
mkSignatureTypeMismatchError bindingName declaredType inferredType =
  "E2005: binding '"
    <> bindingName
    <> "' declared as "
    <> renderType declaredType
    <> " but inferred as "
    <> renderType inferredType

mkApplyTypeError :: ExpressionType -> ExpressionType -> Text
mkApplyTypeError functionType argumentType =
  "E2006: cannot apply function of type "
    <> renderType functionType
    <> " to argument of type "
    <> renderType argumentType

mkListElementTypeMismatchError :: ExpressionType -> ExpressionType -> Text
mkListElementTypeMismatchError expectedType foundType =
  "E2007: list literal elements must have matching types, found "
    <> renderType expectedType
    <> " and "
    <> renderType foundType

mkUnsupportedSectionOperatorError :: Text -> Text
mkUnsupportedSectionOperatorError operatorSymbol =
  "E2008: unsupported operator section '"
    <> operatorSymbol
    <> "'"

mkInvalidSignatureTypeError :: Text -> Text -> Text
mkInvalidSignatureTypeError symbol rawSignature =
  "E2009: invalid or unsupported signature for '"
    <> symbol
    <> "': '"
    <> rawSignature
    <> "'"

mkIfConditionTypeError :: ExpressionType -> Text
mkIfConditionTypeError foundType =
  "E2001: if condition must have type Bool, found " <> renderType foundType

mkIfBranchTypeMismatchError :: ExpressionType -> ExpressionType -> Text
mkIfBranchTypeMismatchError leftType rightType =
  "E2002: if branches must have matching types, found "
    <> renderType leftType
    <> " and "
    <> renderType rightType

renderType :: ExpressionType -> Text
renderType expressionType =
  case expressionType of
    TIntType -> "Int"
    TBoolType -> "Bool"
    TListType elementType -> "[" <> renderType elementType <> "]"
    TFunctionType inputType outputType ->
      renderTypeAtom inputType <> " -> " <> renderType outputType
    TVarType typeVar -> "t" <> Text.pack (show typeVar)

renderTypeAtom :: ExpressionType -> Text
renderTypeAtom expressionType =
  case expressionType of
    TFunctionType _ _ -> "(" <> renderType expressionType <> ")"
    _ -> renderType expressionType

supportsRuntimeEqualityType :: ExpressionType -> Bool
supportsRuntimeEqualityType expressionType =
  -- Keep compile-time acceptance aligned with the currently implemented
  -- runtime equality evaluator to avoid compile/runtime contract drift.
  case expressionType of
    TIntType -> True
    TBoolType -> True
    _ -> False

supportsDeferredEqualityOperandType :: ExpressionType -> Bool
supportsDeferredEqualityOperandType expressionType =
  case expressionType of
    TVarType _ -> True
    _ -> supportsRuntimeEqualityType expressionType
