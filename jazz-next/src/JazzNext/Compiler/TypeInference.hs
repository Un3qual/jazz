{-# LANGUAGE OverloadedStrings #-}

-- | Lightweight type inference layer for the current compiler subset. It
-- canonicalizes the lowered AST, reuses analyzer diagnostics, and adds the
-- small collection of type/runtime-compatibility checks implemented so far.
module JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpressionWithBuiltinsAndHiddenStatements,
    inferExpressionWithBuiltins,
    inferExpression,
    inferExpressionDefault
  ) where

import Data.List (foldl')
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
  ( CaseArm (..),
    ConstraintSignatureType (..),
    DataConstructor (..),
    Expr (..),
    Literal (..),
    Pattern (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol,
    builtinNamesInMode,
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
  ( Identifier,
    identifierText
  )
import JazzNext.Compiler.RecursiveBindings
  ( collectBindingNames,
    freeVarsExprWithBound,
    freeVarsScopeWithBound,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    defaultWarningSettings
  )

-- | `InferenceResult` keeps the canonicalized expression plus analyzer warnings
-- and an `inferredErrors` list that contains both analyzer diagnostics and
-- local type errors discovered during checking.
data InferenceResult = InferenceResult
  { inferredExpr :: Expr,
    inferredWarnings :: [WarningRecord],
    inferredErrors :: [Diagnostic]
  }
  deriving (Eq, Show)

-- This currently forwards analyzer diagnostics while the richer inference/type
-- pipeline is still being built in jazz-next.
inferExpression :: WarningSettings -> Expr -> IO InferenceResult
inferExpression = inferExpressionWithBuiltins ResolveKernelOnly

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
    ELambda parameterName bodyExpr ->
      ELambda parameterName (canonicalizeExpr bodyExpr)
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
    EPatternCase scrutineeExpr caseArms ->
      EPatternCase
        (canonicalizeExpr scrutineeExpr)
        (map canonicalizeCaseArm caseArms)
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

canonicalizeCaseArm :: CaseArm -> CaseArm
canonicalizeCaseArm (CaseArm patternExpr bodyExpr) =
  CaseArm patternExpr (canonicalizeExpr bodyExpr)

canonicalizeStatement :: Statement -> Statement
canonicalizeStatement statement =
  case statement of
    SLet name spanValue valueExpr ->
      SLet name spanValue (canonicalizeExpr valueExpr)
    SSignature name spanValue signaturePayload ->
      SSignature name spanValue signaturePayload
    SData spanValue typeName constructors ->
      SData spanValue typeName constructors
    SModule spanValue modulePath ->
      SModule spanValue modulePath
    SImport spanValue modulePath alias importedSymbols ->
      SImport spanValue modulePath alias importedSymbols
    SExpr spanValue expr ->
      SExpr spanValue (canonicalizeExpr expr)

-- | Internal type language used by the current inferencer.
data ExpressionType
  = TIntType
  | TBoolType
  | TListType ExpressionType
  | TDataType Identifier
  | TFunctionType ExpressionType ExpressionType
  | TVarType Int
  deriving (Eq, Show)

data TypeBinding
  = PlainTypeBinding ExpressionType
  | ConstructorTypeBinding Identifier [ExpressionType]
  deriving (Eq, Show)

type TypeEnv = Map Text TypeBinding

-- | Mutable inference state threaded explicitly through the checker.
data InferState = InferState
  { inferNextTypeVar :: Int,
    inferSubst :: Map Int ExpressionType,
    -- Type variables originating from strict-equality sections must eventually
    -- resolve to runtime-supported equality families.
    inferStrictEqualityVars :: Set Int,
    inferErrorsRev :: [Diagnostic],
    inferErrorCount :: Int
  }

initialInferState :: InferState
initialInferState =
  InferState
    { inferNextTypeVar = 0,
      inferSubst = Map.empty,
      inferStrictEqualityVars = Set.empty,
      inferErrorsRev = [],
      inferErrorCount = 0
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
  TypeEnv ->
  InferState ->
  Expr ->
  (Maybe ExpressionType, InferState)
inferExprType builtinMode env state expr =
  case expr of
    ELit literal -> (Just (literalExpressionType literal), state)
    EVar name ->
      case Map.lookup nameText env of
        Just localType -> instantiateTypeBinding localType state
        Nothing ->
          case instantiateBuiltinType builtinMode nameText state of
            Just (builtinType, nextState) -> (Just builtinType, nextState)
            Nothing -> (Nothing, state)
      where
        nameText = identifierText name
    ELambda parameterName bodyExpr ->
      let (parameterType, stateAfterParameter) = freshTypeVar state
          extendedEnv =
            Map.insert
              (identifierText parameterName)
              (PlainTypeBinding parameterType)
              env
          (bodyType, stateAfterBody) =
            inferExprType builtinMode extendedEnv stateAfterParameter bodyExpr
       in
        case bodyType of
          Just inferredBodyType ->
            ( Just (TFunctionType (resolveType stateAfterBody parameterType) inferredBodyType),
              stateAfterBody
            )
          Nothing -> (Nothing, stateAfterBody)
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
    EPatternCase scrutineeExpr caseArms ->
      let (maybeScrutineeType, stateAfterScrutinee) =
            inferExprType builtinMode env state scrutineeExpr
          (scrutineeType, stateWithScrutineeType) =
            case maybeScrutineeType of
              Just inferredScrutineeType ->
                (inferredScrutineeType, stateAfterScrutinee)
              Nothing ->
                freshTypeVar stateAfterScrutinee
       in inferPatternCaseType builtinMode env scrutineeType stateWithScrutineeType caseArms
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
  TypeEnv ->
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

-- | Scope/type-signature handling for block expressions. This mirrors the
-- statement-order rules enforced by the analyzer while threading inferred types.
inferScopeType :: BuiltinResolutionMode -> TypeEnv -> InferState -> [Statement] -> (Maybe ExpressionType, InferState)
inferScopeType builtinMode initialEnv initialState statements = go initialEnv Nothing Nothing seededState indexedStatements
  where
    indexedStatements = zip [0 ..] statements
    recursiveGroupsByStatement =
      inferRecursiveGroupsOrdered
        (Set.union (Map.keysSet initialEnv) (builtinNamesInMode builtinMode))
        indexedStatements
    selfRecursiveFunctionStatements =
      inferSelfRecursiveBindings exprContainsFunctionBranch indexedStatements
    bindingNamesByStatement = collectBindingNames indexedStatements
    (bindingSeedsByStatement, seededState) =
      allocateBindingSeeds indexedStatements initialState

    go env lastExprType pendingSignatureType state remainingStatements =
      case remainingStatements of
        [] -> (lastExprType, state)
        (statementIndex, statement) : rest ->
          case statement of
            SModule {} ->
              go env lastExprType pendingSignatureType state rest
            SImport {} ->
              go env lastExprType pendingSignatureType state rest
            SData _ typeName constructors ->
              let (nextEnv, nextState) =
                    registerDataConstructors typeName constructors env state
               in go nextEnv lastExprType Nothing nextState rest
            SSignature name signatureSpan signaturePayload ->
              let (nextPendingSignature, nextState) =
                    case signaturePayloadToExpressionType signaturePayload of
                      Just signatureType ->
                        (Just (PendingSignatureType (identifierText name) signatureSpan signatureType), state)
                      Nothing ->
                        ( Nothing,
                          addTypeError
                            state
                            (mkInvalidSignatureTypeError (identifierText name) signatureSpan signaturePayload)
                        )
               in go env lastExprType nextPendingSignature nextState rest
            SLet name bindingSpan valueExpr ->
              let nameText = identifierText name
                  envWithRecursiveBindings =
                    recursiveBindingEnv
                      statementIndex
                      env
                      recursiveGroupsByStatement
                      bindingNamesByStatement
                      bindingSeedsByStatement
                  envWithBindingSeed =
                    case
                        ( Set.member statementIndex selfRecursiveFunctionStatements,
                          Map.lookup statementIndex bindingSeedsByStatement
                        ) of
                      (True, Just bindingSeed) ->
                        Map.insert nameText (PlainTypeBinding bindingSeed) envWithRecursiveBindings
                      _ -> envWithRecursiveBindings
                  envWithPendingSignature =
                    case pendingSignatureType of
                      Just pendingSignature
                        | pendingSignatureName pendingSignature == nameText ->
                            Map.insert
                              nameText
                              (PlainTypeBinding (pendingSignatureDeclaredType pendingSignature))
                              envWithBindingSeed
                      _ -> envWithBindingSeed
                  (valueType, rawStateAfterValue) = inferExprType builtinMode envWithPendingSignature state valueExpr
                  stateAfterValue =
                    annotateNewErrorsWithPrimarySpan bindingSpan state rawStateAfterValue
                  stateAfterBindingSeedCheck =
                    case (Map.lookup statementIndex bindingSeedsByStatement, valueType) of
                      (Just bindingSeed, Just inferredType) ->
                        case unifyTypes bindingSeed inferredType stateAfterValue of
                          Just unifiedState -> unifiedState
                          Nothing ->
                            addTypeError
                              stateAfterValue
                              ( mkBindingTypeMismatchError
                                  nameText
                                  (resolveType stateAfterValue bindingSeed)
                                  bindingSpan
                                  (resolveType stateAfterValue inferredType)
                              )
                      _ -> stateAfterValue
                  stateAfterSignatureCheck =
                    case (pendingSignatureType, valueType) of
                      (Just pendingSignature, Just inferredType)
                        | pendingSignatureName pendingSignature == nameText ->
                            case
                                unifyTypes
                                  (pendingSignatureDeclaredType pendingSignature)
                                  inferredType
                                  stateAfterBindingSeedCheck of
                              Just unifiedState -> unifiedState
                              Nothing ->
                                addTypeError
                                  stateAfterBindingSeedCheck
                                  ( mkSignatureTypeMismatchError
                                      nameText
                                      (pendingSignatureSpan pendingSignature)
                                      (resolveType stateAfterBindingSeedCheck (pendingSignatureDeclaredType pendingSignature))
                                      bindingSpan
                                      (resolveType stateAfterBindingSeedCheck inferredType)
                                  )
                      _ -> stateAfterBindingSeedCheck
                  nextBindingType =
                    case pendingSignatureType of
                      Just pendingSignature
                        | pendingSignatureName pendingSignature == nameText ->
                            Just (resolveType stateAfterSignatureCheck (pendingSignatureDeclaredType pendingSignature))
                      _ ->
                        fmap
                          (resolveType stateAfterSignatureCheck)
                          (Map.lookup statementIndex bindingSeedsByStatement)
                  nextEnv =
                    case nextBindingType of
                      Just inferredType -> Map.insert nameText (PlainTypeBinding inferredType) env
                      Nothing -> env
               in go nextEnv lastExprType Nothing stateAfterSignatureCheck rest
            SExpr exprSpan expr ->
              let (exprType, rawStateAfterExpr) = inferExprType builtinMode env state expr
                  stateAfterExpr =
                    annotateNewErrorsWithPrimarySpan exprSpan state rawStateAfterExpr
               in go env exprType Nothing stateAfterExpr rest

allocateBindingSeeds ::
  [(Int, Statement)] ->
  InferState ->
  (Map Int ExpressionType, InferState)
allocateBindingSeeds indexedStatements initialState =
  foldl' step (Map.empty, initialState) indexedStatements
  where
    step (bindingSeeds, state) (statementIndex, statement) =
      case statement of
        SLet {} ->
          let (bindingSeed, nextState) = freshTypeVar state
           in (Map.insert statementIndex bindingSeed bindingSeeds, nextState)
        _ -> (bindingSeeds, state)

-- Seed self-recursion before branch typing so mixed wrappers like
-- `if True \(x) -> f x else 0` do not skip recursive calls just because only
-- one branch exposes a function value.
exprContainsFunctionBranch :: Expr -> Bool
exprContainsFunctionBranch expr =
  case expr of
    ELambda {} -> True
    EIf _ thenExpr elseExpr ->
      exprContainsFunctionBranch thenExpr
        || exprContainsFunctionBranch elseExpr
    ECase _ thenExpr elseExpr ->
      exprContainsFunctionBranch thenExpr
        || exprContainsFunctionBranch elseExpr
    EPatternCase _ caseArms ->
      any
        (\(CaseArm _ bodyExpr) -> exprContainsFunctionBranch bodyExpr)
        caseArms
    EBlock statements ->
      scopeContainsFunctionBranch statements
    _ -> False

scopeContainsFunctionBranch :: [Statement] -> Bool
scopeContainsFunctionBranch statements =
  case reverse statements of
    SExpr _ expr : _ ->
      exprContainsFunctionBranchViaScopeBindings
        (collectScopeBindingExprs statements)
        Set.empty
        expr
    _ -> False
  where
    -- Mirror runtime block-shape detection so recursive lambda seeding stays
    -- aligned when a block returns a locally-bound lambda alias.
    exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings scopeExpr =
      case scopeExpr of
        EVar bindingName ->
          case Map.lookup (identifierText bindingName) scopeBindings of
            Just bindingExpr
              | Set.notMember (identifierText bindingName) visitedBindings ->
                  exprContainsFunctionBranchViaScopeBindings
                    scopeBindings
                    (Set.insert (identifierText bindingName) visitedBindings)
                    bindingExpr
            _ -> False
        ELambda {} -> True
        EIf _ thenExpr elseExpr ->
          exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings thenExpr
            || exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings elseExpr
        ECase _ thenExpr elseExpr ->
          exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings thenExpr
            || exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings elseExpr
        EPatternCase _ caseArms ->
          any
            (\(CaseArm _ bodyExpr) ->
               exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings bodyExpr
            )
            caseArms
        EBlock nestedStatements ->
          scopeContainsFunctionBranch nestedStatements
        _ -> False

    collectScopeBindingExprs =
      foldl' collect Map.empty
      where
        collect scopeBindings statement =
          case statement of
            SLet bindingName _ valueExpr ->
              Map.insert (identifierText bindingName) valueExpr scopeBindings
            _ -> scopeBindings

recursiveBindingEnv ::
  Int ->
  TypeEnv ->
  Map Int [Int] ->
  Map Int Text ->
  Map Int ExpressionType ->
  TypeEnv
recursiveBindingEnv statementIndex env recursiveGroupsByStatement bindingNamesByStatement bindingSeedsByStatement =
  case Map.lookup statementIndex recursiveGroupsByStatement of
    Nothing -> env
    Just groupMembers ->
      foldl' insertBindingSeed env groupMembers
  where
    -- Preserve the declaration-time snapshot already visible in `env`; only
    -- missing peer names should be seeded into the recursive inference scope.
    insertBindingSeed envAcc memberIndex =
      case
          ( Map.lookup memberIndex bindingNamesByStatement,
            Map.lookup memberIndex bindingSeedsByStatement
          ) of
        (Just bindingNameText, Just bindingSeed)
          | Map.notMember bindingNameText env ->
              Map.insert bindingNameText (PlainTypeBinding bindingSeed) envAcc
        _ -> envAcc

data PendingSignatureType = PendingSignatureType
  { pendingSignatureName :: Text,
    pendingSignatureSpan :: SourceSpan,
    pendingSignatureDeclaredType :: ExpressionType
  }

registerDataConstructors :: Identifier -> [DataConstructor] -> TypeEnv -> InferState -> (TypeEnv, InferState)
registerDataConstructors typeName constructors env initialState =
  foldl' register (env, initialState) constructors
  where
    register (envAcc, stateAcc) (DataConstructor constructorName arity) =
      let (argumentTypes, nextState) = freshTypeVars arity stateAcc
       in
        ( Map.insert
            (identifierText constructorName)
            (ConstructorTypeBinding typeName argumentTypes)
            envAcc,
          nextState
        )

instantiateTypeBinding :: TypeBinding -> InferState -> (Maybe ExpressionType, InferState)
instantiateTypeBinding binding state =
  case binding of
    PlainTypeBinding expressionType ->
      (Just (resolveType state expressionType), state)
    ConstructorTypeBinding typeName argumentTypes ->
      ( Just
          ( foldr
              TFunctionType
              (TDataType typeName)
              (map (resolveType state) argumentTypes)
          ),
        state
      )

freshTypeVars :: Int -> InferState -> ([ExpressionType], InferState)
freshTypeVars count initialState =
  go count [] initialState
  where
    go remaining acc state
      | remaining <= 0 = (reverse acc, state)
      | otherwise =
          let (typeVar, nextState) = freshTypeVar state
           in go (remaining - 1) (typeVar : acc) nextState

-- | Attach the enclosing statement span to diagnostics that were just produced
-- by an inner expression inference step.
annotateNewErrorsWithPrimarySpan :: SourceSpan -> InferState -> InferState -> InferState
annotateNewErrorsWithPrimarySpan spanValue previousState nextState =
  nextState {inferErrorsRev = updatedNewErrors ++ existingErrors}
  where
    previousErrorCount = inferErrorCount previousState
    newErrorCount = inferErrorCount nextState - previousErrorCount
    (newErrors, existingErrors) = splitAt newErrorCount (inferErrorsRev nextState)
    updatedNewErrors = map ensurePrimarySpan newErrors

    ensurePrimarySpan diagnostic =
      case diagnosticPrimarySpan diagnostic of
        Just _ -> diagnostic
        Nothing -> setDiagnosticPrimarySpan spanValue diagnostic

signaturePayloadToExpressionType :: SignaturePayload -> Maybe ExpressionType
signaturePayloadToExpressionType signaturePayload =
  case signaturePayload of
    SignatureType signatureType ->
      Just (signatureTypeToExpressionType signatureType)
    ConstrainedSignature {} ->
      Nothing
    UnsupportedSignature {} ->
      Nothing

signatureTypeToExpressionType :: SignatureType -> ExpressionType
signatureTypeToExpressionType signatureType =
  case signatureType of
    TypeInt -> TIntType
    TypeBool -> TBoolType
    TypeList innerType ->
      TListType (signatureTypeToExpressionType innerType)
    TypeFunction argumentType resultType ->
      TFunctionType
        (signatureTypeToExpressionType argumentType)
        (signatureTypeToExpressionType resultType)

renderSignaturePayload :: SignaturePayload -> Text
renderSignaturePayload signaturePayload =
  case signaturePayload of
    SignatureType signatureType ->
      renderSignatureType signatureType
    ConstrainedSignature constraints signatureType ->
      renderConstrainedSignaturePayload constraints signatureType
    UnsupportedSignature signatureTokens ->
      renderUnsupportedSignatureTokens signatureTokens

renderConstrainedSignaturePayload :: [SignatureConstraint] -> ConstraintSignatureType -> Text
renderConstrainedSignaturePayload constraints signatureType =
  "@{"
    <> Text.intercalate ", " (map renderSignatureConstraint constraints)
    <> "}: "
    <> renderConstraintSignatureType signatureType

renderSignatureConstraint :: SignatureConstraint -> Text
renderSignatureConstraint (SignatureConstraint constraintName arguments) =
  identifierText constraintName
    <> if null arguments
      then ""
      else "(" <> Text.intercalate ", " (map renderConstraintSignatureType arguments) <> ")"

renderConstraintSignatureType :: ConstraintSignatureType -> Text
renderConstraintSignatureType signatureType =
  case signatureType of
    ConstraintTypeName name ->
      identifierText name
    ConstraintTypeApplication name arguments ->
      identifierText name
        <> "("
        <> Text.intercalate ", " (map renderConstraintSignatureType arguments)
        <> ")"
    ConstraintTypeList innerType ->
      "[" <> renderConstraintListElementType innerType <> "]"
    ConstraintTypeFunction argumentType resultType ->
      renderConstraintFunctionArgumentType argumentType <> " -> " <> renderConstraintSignatureType resultType

renderConstraintFunctionArgumentType :: ConstraintSignatureType -> Text
renderConstraintFunctionArgumentType signatureType =
  case signatureType of
    ConstraintTypeFunction {} ->
      "(" <> renderConstraintSignatureType signatureType <> ")"
    _ ->
      renderConstraintSignatureType signatureType

renderConstraintListElementType :: ConstraintSignatureType -> Text
renderConstraintListElementType signatureType =
  case signatureType of
    ConstraintTypeFunction {} ->
      "(" <> renderConstraintSignatureType signatureType <> ")"
    _ ->
      renderConstraintSignatureType signatureType

renderSignatureType :: SignatureType -> Text
renderSignatureType signatureType =
  case signatureType of
    TypeInt -> "Int"
    TypeBool -> "Bool"
    TypeList innerType ->
      "[" <> renderListElementSignatureType innerType <> "]"
    TypeFunction argumentType resultType ->
      renderFunctionArgumentType argumentType <> " -> " <> renderSignatureType resultType

renderFunctionArgumentType :: SignatureType -> Text
renderFunctionArgumentType signatureType =
  case signatureType of
    TypeFunction {} ->
      "(" <> renderSignatureType signatureType <> ")"
    _ ->
      renderSignatureType signatureType

renderListElementSignatureType :: SignatureType -> Text
renderListElementSignatureType signatureType =
  case signatureType of
    TypeFunction {} ->
      "(" <> renderSignatureType signatureType <> ")"
    _ ->
      renderSignatureType signatureType

renderUnsupportedSignatureTokens :: [SignatureToken] -> Text
renderUnsupportedSignatureTokens signatureTokens =
  Text.concat (go Nothing signatureTokens)
  where
    go _ [] = []
    go previousToken (token : rest) =
      let currentToken = renderSignatureToken token
          needsLeadingSpace =
            case previousToken of
              Nothing -> False
              Just previous ->
                tokenNeedsLeadingSpace token
                  && tokenNeedsTrailingSpace previous
          prefix =
            if needsLeadingSpace
              then [" "]
              else []
       in prefix ++ [currentToken] ++ go (Just token) rest

tokenNeedsLeadingSpace :: SignatureToken -> Bool
tokenNeedsLeadingSpace token =
  case token of
    SignatureLParenToken -> False
    SignatureLBracketToken -> False
    SignatureLBraceToken -> False
    SignatureRParenToken -> False
    SignatureRBracketToken -> False
    SignatureRBraceToken -> False
    SignatureCommaToken -> False
    SignatureColonToken -> False
    SignatureArrowToken -> True
    _ -> True

tokenNeedsTrailingSpace :: SignatureToken -> Bool
tokenNeedsTrailingSpace token =
  case token of
    SignatureAtToken -> False
    SignatureLParenToken -> False
    SignatureLBracketToken -> False
    SignatureLBraceToken -> False
    _ -> True

renderSignatureToken :: SignatureToken -> Text
renderSignatureToken token =
  case token of
    SignatureNameToken name -> name
    SignatureIntToken value -> Text.pack (show value)
    SignatureArrowToken -> "->"
    SignatureAtToken -> "@"
    SignatureColonToken -> ":"
    SignatureLParenToken -> "("
    SignatureRParenToken -> ")"
    SignatureLBraceToken -> "{"
    SignatureRBraceToken -> "}"
    SignatureLBracketToken -> "["
    SignatureRBracketToken -> "]"
    SignatureCommaToken -> ","
    SignatureOperatorToken symbol -> symbol
    SignatureOtherToken lexeme -> lexeme

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

-- | Instantiate builtin symbol types on demand so each use site gets fresh type
-- variables instead of sharing one global schematic type.
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

-- | Allocate a fresh type variable for the current inference run.
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
    TDataType typeName -> TDataType typeName
    TFunctionType inputType outputType ->
      TFunctionType
        (applySubstitution subst inputType)
        (applySubstitution subst outputType)
    TVarType typeVar ->
      case Map.lookup typeVar subst of
        Just replacementType -> applySubstitution subst replacementType
        Nothing -> TVarType typeVar

-- | First-order unification over the small internal type language.
unifyTypes :: ExpressionType -> ExpressionType -> InferState -> Maybe InferState
unifyTypes leftType rightType state =
  let resolvedLeft = resolveType state leftType
      resolvedRight = resolveType state rightType
   in case (resolvedLeft, resolvedRight) of
        (TIntType, TIntType) -> Just state
        (TBoolType, TBoolType) -> Just state
        (TDataType leftName, TDataType rightName)
          | leftName == rightName -> Just state
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

-- | Bind a type variable while preserving the deferred equality constraints
-- introduced by strict-equality operator sections.
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
    TDataType {} -> False
    TFunctionType inputType outputType ->
      occursInType typeVar inputType || occursInType typeVar outputType
    TVarType otherVar -> typeVar == otherVar

addTypeError :: InferState -> Diagnostic -> InferState
addTypeError state errorText =
  state
    { inferErrorsRev = errorText : inferErrorsRev state,
      inferErrorCount = inferErrorCount state + 1
    }

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

mkBindingTypeMismatchError :: Text -> ExpressionType -> SourceSpan -> ExpressionType -> Diagnostic
mkBindingTypeMismatchError bindingName expectedType bindingSpan actualType =
  setDiagnosticPrimarySpan
    bindingSpan
    ( setDiagnosticSubject
        bindingName
        ( mkDiagnostic
            "E2006"
            ( "binding '"
                <> bindingName
                <> "' is used recursively as type "
                <> renderType expectedType
                <> " but its definition inferred "
                <> renderType actualType
            )
        )
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

mkPatternTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkPatternTypeMismatchError scrutineeType patternType =
  mkDiagnostic
    "E2011"
    ( "case pattern of type "
        <> renderType patternType
        <> " does not match scrutinee type "
        <> renderType scrutineeType
    )

mkPatternBranchTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkPatternBranchTypeMismatchError leftType rightType =
  mkDiagnostic
    "E2012"
    ( "case arms must have matching types, found "
        <> renderType leftType
        <> " and "
        <> renderType rightType
    )

mkInvalidSignatureTypeError :: Text -> SourceSpan -> SignaturePayload -> Diagnostic
mkInvalidSignatureTypeError symbol signatureSpan signaturePayload =
  setDiagnosticSubject symbol $
    setDiagnosticPrimarySpan
      signatureSpan
      ( mkDiagnostic
          "E2009"
          ( "invalid or unsupported signature for '"
              <> symbol
              <> "': '"
              <> renderSignaturePayload signaturePayload
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
    TDataType typeName -> identifierText typeName
    TFunctionType inputType outputType ->
      renderTypeAtom inputType <> " -> " <> renderType outputType
    TVarType typeVar -> "t" <> Text.pack (show typeVar)

renderTypeAtom :: ExpressionType -> Text
renderTypeAtom expressionType =
  case expressionType of
    TFunctionType _ _ -> "(" <> renderType expressionType <> ")"
    _ -> renderType expressionType

extendBoundWithPattern :: Pattern -> Set Text -> Set Text
extendBoundWithPattern pattern bound =
  case pattern of
    PVariable name -> Set.insert (identifierText name) bound
    PWildcard -> bound
    PLiteral {} -> bound
    PConstructor _ patterns ->
      foldl' (flip extendBoundWithPattern) bound patterns
    PList patterns ->
      foldl' (flip extendBoundWithPattern) bound patterns

inferPatternCaseType ::
  BuiltinResolutionMode ->
  TypeEnv ->
  ExpressionType ->
  InferState ->
  [CaseArm] ->
  (Maybe ExpressionType, InferState)
inferPatternCaseType builtinMode env scrutineeType initialState caseArms =
  foldl' step (Nothing, initialState) caseArms
  where
    step ::
      (Maybe ExpressionType, InferState) ->
      CaseArm ->
      (Maybe ExpressionType, InferState)
    step (maybeExpectedBodyType, stateAcc) (CaseArm pattern bodyExpr) =
      let (rawPatternTyping, stateAfterPatternCheck) =
            inferPatternType env scrutineeType pattern stateAcc
          (patternTyping, stateAfterPattern) =
            rejectDuplicatePatternBinders pattern rawPatternTyping stateAfterPatternCheck
       in
        if patternSkipsBranchType patternTyping
          then (maybeExpectedBodyType, stateAfterPattern)
          else
            let armEnv =
                  patternBindings patternTyping `Map.union` env
                (maybeBodyType, stateAfterBody) =
                  inferExprType builtinMode armEnv stateAfterPattern bodyExpr
             in
              case (maybeExpectedBodyType, maybeBodyType) of
                (Nothing, _) ->
                  (fmap (resolveType stateAfterBody) maybeBodyType, stateAfterBody)
                (expectedBodyType, Nothing) ->
                  (expectedBodyType, stateAfterBody)
                (Just inferredExpectedBodyType, Just inferredBodyType) ->
                  case unifyTypes inferredExpectedBodyType inferredBodyType stateAfterBody of
                    Just unifiedState ->
                      (Just (resolveType unifiedState inferredExpectedBodyType), unifiedState)
                    Nothing ->
                      ( Just inferredExpectedBodyType,
                        addTypeError
                          stateAfterBody
                          ( mkPatternBranchTypeMismatchError
                              (resolveType stateAfterBody inferredExpectedBodyType)
                              (resolveType stateAfterBody inferredBodyType)
                          )
                      )

data PatternTyping = PatternTyping
  { patternBindings :: TypeEnv,
    patternSkipsBranchType :: Bool
  }

emptyPatternTyping :: PatternTyping
emptyPatternTyping =
  PatternTyping
    { patternBindings = Map.empty,
      patternSkipsBranchType = False
    }

skipBranchPatternTyping :: PatternTyping
skipBranchPatternTyping =
  emptyPatternTyping {patternSkipsBranchType = True}

mergePatternTyping :: PatternTyping -> PatternTyping -> PatternTyping
mergePatternTyping left right =
  PatternTyping
    { patternBindings = patternBindings left `Map.union` patternBindings right,
      patternSkipsBranchType =
        patternSkipsBranchType left || patternSkipsBranchType right
    }

rejectDuplicatePatternBinders :: Pattern -> PatternTyping -> InferState -> (PatternTyping, InferState)
rejectDuplicatePatternBinders pattern typing state =
  foldl' reject (typing, state) (patternDuplicateBinderNames pattern)
  where
    reject (typingAcc, stateAcc) duplicateName =
      ( typingAcc {patternSkipsBranchType = True},
        addTypeError stateAcc (mkDuplicatePatternBinderError duplicateName)
      )

patternDuplicateBinderNames :: Pattern -> [Text]
patternDuplicateBinderNames pattern =
  Set.toList duplicates
  where
    (_, duplicates) = collect pattern Set.empty Set.empty

    collect :: Pattern -> Set Text -> Set Text -> (Set Text, Set Text)
    collect candidate seen duplicatesAcc =
      case candidate of
        PVariable name ->
          let nameText = identifierText name
           in
            if Set.member nameText seen
              then (seen, Set.insert nameText duplicatesAcc)
              else (Set.insert nameText seen, duplicatesAcc)
        PWildcard -> (seen, duplicatesAcc)
        PLiteral {} -> (seen, duplicatesAcc)
        PConstructor _ nestedPatterns ->
          collectNested seen duplicatesAcc nestedPatterns
        PList nestedPatterns ->
          collectNested seen duplicatesAcc nestedPatterns

    collectNested seen duplicatesAcc =
      foldl'
        (\(seenAcc, duplicatesAcc') nestedPattern ->
           collect nestedPattern seenAcc duplicatesAcc'
        )
        (seen, duplicatesAcc)

inferPatternType :: TypeEnv -> ExpressionType -> Pattern -> InferState -> (PatternTyping, InferState)
inferPatternType env scrutineeType pattern state =
  case pattern of
    PVariable name ->
      ( emptyPatternTyping
          { patternBindings =
              Map.singleton
                (identifierText name)
                (PlainTypeBinding (resolveType state scrutineeType))
          },
        state
      )
    PWildcard -> (emptyPatternTyping, state)
    PLiteral literal ->
      let literalType = literalExpressionType literal
       in case unifyTypes scrutineeType literalType state of
            Just unifiedState -> (emptyPatternTyping, unifiedState)
            Nothing ->
              ( skipBranchPatternTyping,
                addTypeError
                  state
                  ( mkPatternTypeMismatchError
                      (resolveType state scrutineeType)
                      literalType
                  )
              )
    PConstructor constructorName patterns ->
      inferConstructorPatternType env scrutineeType constructorName patterns state
    PList patterns ->
      inferListPatternType env scrutineeType patterns state

inferConstructorPatternType ::
  TypeEnv ->
  ExpressionType ->
  Identifier ->
  [Pattern] ->
  InferState ->
  (PatternTyping, InferState)
inferConstructorPatternType env scrutineeType constructorName patterns state =
  case Map.lookup constructorNameText env of
    Just (ConstructorTypeBinding typeName argumentTypes) ->
      let expectedArity = length argumentTypes
       in
        if expectedArity /= length patterns
          then
            ( skipBranchPatternTyping,
              addTypeError
                state
                (mkConstructorPatternArityError constructorNameText expectedArity (length patterns))
            )
          else
            let constructorResultType = TDataType typeName
             in
              case unifyTypes scrutineeType constructorResultType state of
                Just stateAfterResultCheck ->
                  inferConstructorArgumentPatterns
                    env
                    (map (resolveType stateAfterResultCheck) argumentTypes)
                    patterns
                    stateAfterResultCheck
                Nothing ->
                  ( skipBranchPatternTyping,
                    addTypeError
                      state
                      ( mkPatternTypeMismatchError
                          (resolveType state scrutineeType)
                          constructorResultType
                      )
                  )
    _ ->
      ( skipBranchPatternTyping,
        addTypeError
          state
          (mkUnknownConstructorPatternError constructorNameText)
      )
  where
    constructorNameText = identifierText constructorName

inferConstructorArgumentPatterns ::
  TypeEnv ->
  [ExpressionType] ->
  [Pattern] ->
  InferState ->
  (PatternTyping, InferState)
inferConstructorArgumentPatterns env argumentTypes patterns initialState =
  go emptyPatternTyping initialState (zip argumentTypes patterns)
  where
    go typingAcc stateAcc remainingPatterns =
      case remainingPatterns of
        [] -> (typingAcc, stateAcc)
        (argumentType, pattern) : rest ->
          let (typing, stateAfterPattern) =
                inferPatternType env argumentType pattern stateAcc
              mergedTyping = mergePatternTyping typing typingAcc
           in
            if patternSkipsBranchType mergedTyping
              then (mergedTyping, rollbackSkippedPatternState initialState stateAfterPattern)
              else go mergedTyping stateAfterPattern rest

inferListPatternType ::
  TypeEnv ->
  ExpressionType ->
  [Pattern] ->
  InferState ->
  (PatternTyping, InferState)
inferListPatternType env scrutineeType patterns state =
  let (elementType, stateWithElementType) = freshTypeVar state
      listPatternType = TListType elementType
      stateAfterListCheck =
        case unifyTypes scrutineeType listPatternType stateWithElementType of
          Just unifiedState -> unifiedState
          Nothing ->
            addTypeError
              stateWithElementType
              ( mkPatternTypeMismatchError
                  (resolveType stateWithElementType scrutineeType)
                  (resolveType stateWithElementType listPatternType)
              )
   in
    if hasNewPatternError stateWithElementType stateAfterListCheck
      then (skipBranchPatternTyping, stateAfterListCheck)
      else
        inferListElementPatterns
          env
          (resolveType stateAfterListCheck elementType)
          patterns
          stateAfterListCheck

inferListElementPatterns ::
  TypeEnv ->
  ExpressionType ->
  [Pattern] ->
  InferState ->
  (PatternTyping, InferState)
inferListElementPatterns env elementType patterns initialState =
  go emptyPatternTyping initialState patterns
  where
    go typingAcc stateAcc remainingPatterns =
      case remainingPatterns of
        [] -> (typingAcc, stateAcc)
        pattern : rest ->
          let (typing, stateAfterPattern) =
                inferPatternType env elementType pattern stateAcc
              mergedTyping = mergePatternTyping typing typingAcc
           in
            if patternSkipsBranchType mergedTyping
              then (mergedTyping, rollbackSkippedPatternState initialState stateAfterPattern)
              else go mergedTyping stateAfterPattern rest

rollbackSkippedPatternState :: InferState -> InferState -> InferState
rollbackSkippedPatternState stableState failedState =
  stableState
    { inferErrorsRev = inferErrorsRev failedState,
      inferErrorCount = inferErrorCount failedState
    }

hasNewPatternError :: InferState -> InferState -> Bool
hasNewPatternError previousState nextState =
  inferErrorCount nextState > inferErrorCount previousState

mkConstructorPatternArityError :: Text -> Int -> Int -> Diagnostic
mkConstructorPatternArityError constructorName expectedArity actualArity =
  mkDiagnostic
    "E2011"
    ( "constructor case pattern '"
        <> constructorName
        <> "' expects "
        <> Text.pack (show expectedArity)
        <> " argument(s), found "
        <> Text.pack (show actualArity)
    )

mkUnknownConstructorPatternError :: Text -> Diagnostic
mkUnknownConstructorPatternError constructorName =
  mkDiagnostic
    "E2011"
    ("unknown constructor case pattern '" <> constructorName <> "'")

mkDuplicatePatternBinderError :: Text -> Diagnostic
mkDuplicatePatternBinderError binderName =
  mkDiagnostic
    "E2011"
    ("duplicate case pattern binder '" <> binderName <> "'")

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
