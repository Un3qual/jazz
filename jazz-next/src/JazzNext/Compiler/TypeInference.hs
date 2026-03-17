{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpressionWithBuiltinsAndHiddenStatements,
    inferExpressionWithBuiltins,
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
    analyzeProgramWithBuiltinsAndHiddenStatements
  )
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol,
    builtinSymbolName,
    lookupBuiltinSymbolInMode
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic (..),
    SourceSpan,
    WarningRecord,
    mkDiagnostic,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject
  )
import JazzNext.Compiler.Identifier
  ( identifierText
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    defaultWarningSettings
  )

data InferenceResult = InferenceResult
  { inferredExpr :: Expr,
    inferredWarnings :: [WarningRecord],
    inferredErrors :: [Diagnostic]
  }
  deriving (Eq, Show)

-- This currently forwards analyzer diagnostics while the richer inference/type
-- pipeline is still being built in jazz-next.
inferExpression :: WarningSettings -> Expr -> IO InferenceResult
inferExpression = inferExpressionWithBuiltins ResolveCompatibility

inferExpressionWithBuiltins :: BuiltinResolutionMode -> WarningSettings -> Expr -> IO InferenceResult
inferExpressionWithBuiltins builtinMode =
  inferExpressionWithBuiltinsAndHiddenStatements builtinMode Set.empty

inferExpressionWithBuiltinsAndHiddenStatements ::
  BuiltinResolutionMode ->
  Set Int ->
  WarningSettings ->
  Expr ->
  IO InferenceResult
inferExpressionWithBuiltinsAndHiddenStatements builtinMode hiddenStatementIndices settings expr = do
  let canonicalExpr = canonicalizeExpr expr
  AnalysisResult _ warnings errors <-
    analyzeProgramWithBuiltinsAndHiddenStatements
      builtinMode
      hiddenStatementIndices
      settings
      canonicalExpr
  let typeErrors = collectExprTypeErrors builtinMode canonicalExpr
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
    ELit literal -> ELit literal
    EVar name -> EVar name
    EOperatorValue operatorSymbol -> EOperatorValue operatorSymbol
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
    EBinary operatorSymbol leftExpr rightExpr
      | operatorSymbol == "$" ->
          EApply
            (canonicalizeExpr leftExpr)
            (canonicalizeExpr rightExpr)
      | otherwise ->
          EBinary
            operatorSymbol
            (canonicalizeExpr leftExpr)
            (canonicalizeExpr rightExpr)
    ESectionLeft leftExpr operatorSymbol ->
      ESectionLeft (canonicalizeExpr leftExpr) operatorSymbol
    ESectionRight operatorSymbol rightExpr ->
      ESectionRight operatorSymbol (canonicalizeExpr rightExpr)
    EBlock statements -> EBlock (map canonicalizeStatement statements)

canonicalizeStatement :: Statement -> Statement
canonicalizeStatement statement =
  case statement of
    SLet name spanValue valueExpr ->
      SLet name spanValue (canonicalizeExpr valueExpr)
    SSignature name spanValue signatureText ->
      SSignature name spanValue signatureText
    SModule spanValue modulePath ->
      SModule spanValue modulePath
    SImport spanValue modulePath alias importedSymbols ->
      SImport spanValue modulePath alias importedSymbols
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
    inferErrorsRev :: [Diagnostic]
  }

initialInferState :: InferState
initialInferState =
  InferState
    { inferNextTypeVar = 0,
      inferSubst = Map.empty,
      inferStrictEqualityVars = Set.empty,
      inferErrorsRev = []
    }

collectExprTypeErrors :: BuiltinResolutionMode -> Expr -> [Diagnostic]
collectExprTypeErrors builtinMode expr =
  let (_, finalState) =
        inferExprType
          builtinMode
          Map.empty
          initialInferState
          expr
   in reverse (inferErrorsRev finalState)

-- Core expressions do not retain inner-node source spans yet, so inference
-- reuses the enclosing statement span as the best available location metadata.
inferExprType ::
  BuiltinResolutionMode ->
  Map Text ExpressionType ->
  InferState ->
  Expr ->
  (Maybe ExpressionType, InferState)
inferExprType builtinMode env state expr =
  case expr of
    ELit literal -> (Just (literalExpressionType literal), state)
    EVar name ->
      case Map.lookup nameText env of
        Just localType -> (Just (resolveType state localType), state)
        Nothing ->
          case instantiateBuiltinType builtinMode nameText state of
            Just (builtinType, nextState) -> (Just builtinType, nextState)
            Nothing -> (Nothing, state)
      where
        nameText = identifierText name
    EOperatorValue operatorSymbol ->
      case instantiateOperatorType operatorSymbol state of
        Just (operatorType, nextState) -> (Just operatorType, nextState)
        Nothing ->
          ( Nothing,
            addTypeError
              state
              (mkUnsupportedOperatorValueError operatorSymbol)
          )
    EList elements -> inferListType builtinMode env state elements
    EApply functionExpr argumentExpr ->
      let (functionType, stateAfterFunction) =
            inferExprType builtinMode env state functionExpr
          (argumentType, stateAfterArgument) =
            inferExprType builtinMode env stateAfterFunction argumentExpr
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
      inferExprType builtinMode env state (ECase conditionExpr thenExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr ->
      let (conditionType, stateAfterCondition) =
            inferExprType builtinMode env state conditionExpr
          (thenType, stateAfterThen) =
            inferExprType builtinMode env stateAfterCondition thenExpr
          (elseType, stateAfterElse) =
            inferExprType builtinMode env stateAfterThen elseExpr
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
      let (leftType, stateAfterLeft) =
            inferExprType builtinMode env state leftExpr
          (rightType, stateAfterRight) =
            inferExprType builtinMode env stateAfterLeft rightExpr
       in case (leftType, rightType) of
            (Just inferredLeftType, Just inferredRightType) ->
              inferBinaryType
                operatorSymbol
                inferredLeftType
                inferredRightType
                stateAfterRight
            _ -> (Nothing, stateAfterRight)
    ESectionLeft leftExpr operatorSymbol ->
      let (leftType, stateAfterLeft) =
            inferExprType builtinMode env state leftExpr
       in case leftType of
            Just inferredLeftType ->
              inferSectionLeftType
                operatorSymbol
                inferredLeftType
                stateAfterLeft
            Nothing -> (Nothing, stateAfterLeft)
    ESectionRight operatorSymbol rightExpr ->
      let (rightType, stateAfterRight) =
            inferExprType builtinMode env state rightExpr
       in case rightType of
            Just inferredRightType ->
              inferSectionRightType
                operatorSymbol
                inferredRightType
                stateAfterRight
            Nothing -> (Nothing, stateAfterRight)
    EBlock statements -> inferScopeType builtinMode env state statements

literalExpressionType :: Literal -> ExpressionType
literalExpressionType literal =
  case literal of
    LInt _ -> TIntType
    LBool _ -> TBoolType

inferListType ::
  BuiltinResolutionMode ->
  Map Text ExpressionType ->
  InferState ->
  [Expr] ->
  (Maybe ExpressionType, InferState)
inferListType builtinMode env state elements =
  case elements of
    [] ->
      let (elementType, nextState) = freshTypeVar state
       in (Just (TListType elementType), nextState)
    firstElement : restElements ->
      let (firstType, stateAfterFirst) =
            inferExprType builtinMode env state firstElement
          (finalElementType, finalState) =
            foldl
              step
              (firstType, stateAfterFirst)
              restElements
       in (TListType <$> finalElementType, finalState)
  where
    step :: (Maybe ExpressionType, InferState) -> Expr -> (Maybe ExpressionType, InferState)
    step (expectedType, stateAcc) element =
      let (actualType, stateAfterElement) =
            inferExprType builtinMode env stateAcc element
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
  | ApplicationRule

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
    "$" -> Just ApplicationRule
    _ -> Nothing

inferBinaryType ::
  Text ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
inferBinaryType operatorSymbol leftType rightType state =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule resultType) ->
      applyNumericBinaryRule operatorSymbol resultType leftType rightType state
    Just StrictEqualityRule ->
      applyStrictEqualityBinaryRule operatorSymbol leftType rightType state
    Just ApplicationRule ->
      applyApplicationBinaryRule leftType rightType state
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

applyApplicationBinaryRule ::
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyApplicationBinaryRule functionType argumentType state =
  let (resultTypeVar, stateAfterResultVar) = freshTypeVar state
   in case unifyTypes functionType (TFunctionType argumentType resultTypeVar) stateAfterResultVar of
        Just unifiedState ->
          (Just (resolveType unifiedState resultTypeVar), unifiedState)
        Nothing ->
          ( Nothing,
            addTypeError
              stateAfterResultVar
              ( mkApplyTypeError
                  (resolveType stateAfterResultVar functionType)
                  (resolveType stateAfterResultVar argumentType)
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

inferScopeType :: BuiltinResolutionMode -> Map Text ExpressionType -> InferState -> [Statement] -> (Maybe ExpressionType, InferState)
inferScopeType builtinMode initialEnv initialState statements = go initialEnv Nothing Nothing initialState statements
  where
    go env lastExprType pendingSignatureType state remainingStatements =
      case remainingStatements of
        [] -> (lastExprType, state)
        statement : rest ->
          case statement of
            SModule {} ->
              go env lastExprType pendingSignatureType state rest
            SImport {} ->
              go env lastExprType pendingSignatureType state rest
            SSignature name signatureSpan signatureText ->
              let (nextPendingSignature, nextState) =
                    case parseSignatureType signatureText of
                      Just signatureType ->
                        (Just (PendingSignatureType (identifierText name) signatureSpan signatureType), state)
                      Nothing ->
                        ( Nothing,
                          addTypeError
                            state
                            (mkInvalidSignatureTypeError (identifierText name) signatureSpan signatureText)
                        )
               in go env lastExprType nextPendingSignature nextState rest
            SLet name bindingSpan valueExpr ->
              let nameText = identifierText name
                  envWithPendingSignature =
                    case pendingSignatureType of
                      Just pendingSignature
                        | pendingSignatureName pendingSignature == nameText ->
                            Map.insert
                              nameText
                              (pendingSignatureDeclaredType pendingSignature)
                              env
                      _ -> env
                  (valueType, rawStateAfterValue) = inferExprType builtinMode envWithPendingSignature state valueExpr
                  stateAfterValue =
                    annotateNewErrorsWithPrimarySpan bindingSpan state rawStateAfterValue
                  stateAfterSignatureCheck =
                    case (pendingSignatureType, valueType) of
                      (Just pendingSignature, Just inferredType)
                        | pendingSignatureName pendingSignature == nameText ->
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
                                      nameText
                                      (pendingSignatureSpan pendingSignature)
                                      (resolveType stateAfterValue (pendingSignatureDeclaredType pendingSignature))
                                      bindingSpan
                                      (resolveType stateAfterValue inferredType)
                                  )
                      _ -> stateAfterValue
                  nextBindingType =
                    case pendingSignatureType of
                      Just pendingSignature
                        | pendingSignatureName pendingSignature == nameText ->
                            Just (resolveType stateAfterSignatureCheck (pendingSignatureDeclaredType pendingSignature))
                      _ -> fmap (resolveType stateAfterSignatureCheck) valueType
                  nextEnv =
                    case nextBindingType of
                      Just inferredType -> Map.insert nameText inferredType env
                      Nothing -> env
               in go nextEnv lastExprType Nothing stateAfterSignatureCheck rest
            SExpr exprSpan expr ->
              let (exprType, rawStateAfterExpr) = inferExprType builtinMode env state expr
                  stateAfterExpr =
                    annotateNewErrorsWithPrimarySpan exprSpan state rawStateAfterExpr
               in go env exprType Nothing stateAfterExpr rest

data PendingSignatureType = PendingSignatureType
  { pendingSignatureName :: Text,
    pendingSignatureSpan :: SourceSpan,
    pendingSignatureDeclaredType :: ExpressionType
  }

annotateNewErrorsWithPrimarySpan :: SourceSpan -> InferState -> InferState -> InferState
annotateNewErrorsWithPrimarySpan spanValue previousState nextState =
  nextState {inferErrorsRev = updatedNewErrors ++ existingErrors}
  where
    previousErrorCount = length (inferErrorsRev previousState)
    newErrorCount = length (inferErrorsRev nextState) - previousErrorCount
    (newErrors, existingErrors) = splitAt newErrorCount (inferErrorsRev nextState)
    updatedNewErrors = map ensurePrimarySpan newErrors

    ensurePrimarySpan diagnostic =
      case diagnosticPrimarySpan diagnostic of
        Just _ -> diagnostic
        Nothing -> setDiagnosticPrimarySpan spanValue diagnostic

parseSignatureType :: Text -> Maybe ExpressionType
parseSignatureType signatureText =
  case Text.filter (not . isSpace) signatureText of
    "Int" -> Just TIntType
    "Bool" -> Just TBoolType
    _ -> Nothing

instantiateBuiltinType :: BuiltinResolutionMode -> Text -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinType builtinMode name state =
  case lookupBuiltinSymbolInMode builtinMode name of
    Just builtinSymbol -> instantiateBuiltinSymbolType builtinSymbol state
    Nothing -> Nothing

instantiateOperatorType :: Text -> InferState -> Maybe (ExpressionType, InferState)
instantiateOperatorType operatorSymbol state =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule resultType) ->
      Just (TFunctionType TIntType (TFunctionType TIntType resultType), state)
    Just StrictEqualityRule ->
      let (operandType, stateAfterOperandType) = freshTypeVar state
       in
        case operandType of
          TVarType typeVar ->
            Just
              ( TFunctionType operandType (TFunctionType operandType TBoolType),
                addStrictEqualityTypeVarConstraint typeVar stateAfterOperandType
              )
          _ -> Nothing
    Just ApplicationRule ->
      let (argumentType, stateAfterArgumentType) = freshTypeVar state
          (resultType, stateAfterResultType) = freshTypeVar stateAfterArgumentType
       in
        Just
          ( TFunctionType
              (TFunctionType argumentType resultType)
              (TFunctionType argumentType resultType),
            stateAfterResultType
          )
    Nothing -> Nothing

instantiateBuiltinSymbolType :: BuiltinSymbol -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinSymbolType builtinSymbol state =
  -- Use catalog names here so newly-added symbols safely fall back to `Nothing`
  -- until an explicit type-instantiation rule is defined.
  case builtinSymbolName builtinSymbol of
    "hd" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (TFunctionType (TListType elementType) elementType, stateAfterElement)
    "tl" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in Just (TFunctionType (TListType elementType) (TListType elementType), stateAfterElement)
    "map" ->
      let (sourceType, stateAfterSource) = freshTypeVar state
          (targetType, stateAfterTarget) = freshTypeVar stateAfterSource
       in
        Just
          ( TFunctionType
              (TFunctionType sourceType targetType)
              (TFunctionType (TListType sourceType) (TListType targetType)),
            stateAfterTarget
          )
    "filter" ->
      let (elementType, stateAfterElement) = freshTypeVar state
       in
        Just
          ( TFunctionType
              (TFunctionType elementType TBoolType)
              (TFunctionType (TListType elementType) (TListType elementType)),
            stateAfterElement
          )
    "print!" ->
      -- Stub-v1 runtime keeps `print!` as an impure primitive that returns the
      -- evaluated argument value unchanged so compile/runtime paths stay simple.
      let (valueType, stateAfterValueType) = freshTypeVar state
       in Just (TFunctionType valueType valueType, stateAfterValueType)
    _ -> Nothing

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

addTypeError :: InferState -> Diagnostic -> InferState
addTypeError state errorText =
  state {inferErrorsRev = errorText : inferErrorsRev state}

addStrictEqualityTypeVarConstraint :: Int -> InferState -> InferState
addStrictEqualityTypeVarConstraint typeVar state =
  state
    { inferStrictEqualityVars =
        Set.insert typeVar (inferStrictEqualityVars state)
    }

mkBinaryTypeError :: Text -> ExpressionType -> ExpressionType -> Diagnostic
mkBinaryTypeError operatorSymbol leftType rightType =
  mkDiagnostic
    "E2003"
    ( "cannot apply operator '"
        <> operatorSymbol
        <> "' to operands of type "
        <> renderType leftType
        <> " and "
        <> renderType rightType
    )

mkStrictEqualityTypeError :: Text -> ExpressionType -> ExpressionType -> Diagnostic
mkStrictEqualityTypeError operatorSymbol leftType rightType =
  mkDiagnostic
    "E2004"
    ( "strict equality operator '"
        <> operatorSymbol
        <> "' requires operands of the same type, found "
        <> renderType leftType
        <> " and "
        <> renderType rightType
    )

mkStrictEqualityUnsupportedTypeError :: Text -> ExpressionType -> Diagnostic
mkStrictEqualityUnsupportedTypeError operatorSymbol foundType =
  mkDiagnostic
    "E2004"
    ( "strict equality operator '"
        <> operatorSymbol
        <> "' is only supported for Int and Bool operands, found "
        <> renderType foundType
    )

mkSignatureTypeMismatchError ::
  Text ->
  SourceSpan ->
  ExpressionType ->
  SourceSpan ->
  ExpressionType ->
  Diagnostic
mkSignatureTypeMismatchError bindingName signatureSpan declaredType bindingSpan inferredType =
  setDiagnosticSubject bindingName $
    setDiagnosticRelatedSpan
      bindingSpan
      ( setDiagnosticPrimarySpan
          signatureSpan
          ( mkDiagnostic
              "E2005"
              ( "binding '"
                  <> bindingName
                  <> "' declared as "
                  <> renderType declaredType
                  <> " but inferred as "
                  <> renderType inferredType
              )
          )
      )

mkApplyTypeError :: ExpressionType -> ExpressionType -> Diagnostic
mkApplyTypeError functionType argumentType =
  mkDiagnostic
    "E2006"
    ( "cannot apply function of type "
        <> renderType functionType
        <> " to argument of type "
        <> renderType argumentType
    )

mkListElementTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkListElementTypeMismatchError expectedType foundType =
  mkDiagnostic
    "E2007"
    ( "list literal elements must have matching types, found "
        <> renderType expectedType
        <> " and "
        <> renderType foundType
    )

mkUnsupportedSectionOperatorError :: Text -> Diagnostic
mkUnsupportedSectionOperatorError operatorSymbol =
  mkDiagnostic "E2008" ("unsupported operator section '" <> operatorSymbol <> "'")

mkUnsupportedOperatorValueError :: Text -> Diagnostic
mkUnsupportedOperatorValueError operatorSymbol =
  mkDiagnostic "E2010" ("unsupported operator value '" <> operatorSymbol <> "'")

mkInvalidSignatureTypeError :: Text -> SourceSpan -> Text -> Diagnostic
mkInvalidSignatureTypeError symbol signatureSpan rawSignature =
  setDiagnosticSubject symbol $
    setDiagnosticPrimarySpan
      signatureSpan
      ( mkDiagnostic
          "E2009"
          ( "invalid or unsupported signature for '"
              <> symbol
              <> "': '"
              <> rawSignature
              <> "'"
          )
      )

mkIfConditionTypeError :: ExpressionType -> Diagnostic
mkIfConditionTypeError foundType =
  mkDiagnostic "E2001" ("if condition must have type Bool, found " <> renderType foundType)

mkIfBranchTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkIfBranchTypeMismatchError leftType rightType =
  mkDiagnostic
    "E2002"
    ( "if branches must have matching types, found "
        <> renderType leftType
        <> " and "
        <> renderType rightType
    )

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
