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

import Control.Applicative ((<|>))
import Data.List (foldl')
import Data.Maybe (isJust, isNothing)
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
    ClassMethodSignature (..),
    ConstraintSignatureType (..),
    DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    NumericType (..),
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
    builtinSymbolNumericConversionTarget,
    lookupBuiltinSymbolInMode,
    numericTypeFloatIntegerBounds,
    numericTypeFloatMax,
    numericTypeFromName,
    numericTypeIntegerBounds,
    numericTypeIsIntegral,
    numericTypeLiteralIntegerBounds,
    numericTypeSupportsRuntimeArithmetic,
    numericTypeSupportsRuntimeComparison,
    renderNumericTypeName
  )
import JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    concreteImplFactKey,
    concreteImplFactClassName,
    constraintFunctionArgumentTypes,
    constraintImplFactKey,
    constraintSignatureAliasVariants,
    constraintSignatureTypeContainsClassParameter,
    constraintSignatureTypesCompatible,
    identifierLooksLikeTypeVariable,
    normalizeConstraintSignatureName,
    qualifiedMethodKey,
    renderConstraintSignatureType,
    signaturePayloadConstraintType,
    splitQualifiedMethodKey,
    substituteClassMethodSignature
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
import JazzNext.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralIntegralValue
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText,
    mkIdentifier,
    operatorBindingIdentifierText,
    qualifiedIdentifierText
  )
import JazzNext.Compiler.RecursiveBindings
  ( collectBindingNames,
    freeVarsExprWithBound,
    freeVarsScopeWithBound,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings
  )
import JazzNext.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol
  )
import JazzNext.Compiler.RuntimeHints
  ( BindingRuntimeHintKey,
    bindingRuntimeHintKeyInModule
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
    inferredErrors :: [Diagnostic],
    inferredRuntimeTypeHints :: Map BindingRuntimeHintKey ConstraintSignatureType
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
  let (typeErrors, runtimeTypeHints) = collectExprTypeInfo builtinMode canonicalExpr
  pure
    InferenceResult
      { inferredExpr = canonicalExpr,
        inferredWarnings = warnings,
        inferredErrors = errors ++ typeErrors,
        inferredRuntimeTypeHints = runtimeTypeHints
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
    ETuple elements -> ETuple (map canonicalizeExpr elements)
    EApply functionExpr argumentExpr ->
      EApply (canonicalizeExpr functionExpr) (canonicalizeExpr argumentExpr)
    ETypeApplication functionExpr signatureType ->
      ETypeApplication (canonicalizeExpr functionExpr) signatureType
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
canonicalizeCaseArm (CaseArm patternExpr guardExpr bodyExpr) =
  CaseArm patternExpr (fmap canonicalizeExpr guardExpr) (canonicalizeExpr bodyExpr)

canonicalizeStatement :: Statement -> Statement
canonicalizeStatement statement =
  case statement of
    SLet name spanValue valueExpr ->
      SLet name spanValue (canonicalizeExpr valueExpr)
    SSignature name spanValue signaturePayload ->
      SSignature name spanValue signaturePayload
    SData spanValue typeName typeParameters constructors ->
      SData spanValue typeName typeParameters constructors
    SClass spanValue capabilityName parameters methods ->
      SClass spanValue capabilityName parameters methods
    SImpl spanValue capabilityName arguments methods ->
      SImpl spanValue capabilityName arguments (map canonicalizeImplMethod methods)
    SModule spanValue modulePath ->
      SModule spanValue modulePath
    SImport spanValue modulePath alias importedSymbols ->
      SImport spanValue modulePath alias importedSymbols
    SExpr spanValue expr ->
      SExpr spanValue (canonicalizeExpr expr)

canonicalizeImplMethod :: ImplMethod -> ImplMethod
canonicalizeImplMethod (ImplMethod methodName spanValue methodExpr) =
  ImplMethod methodName spanValue (canonicalizeExpr methodExpr)

-- | Internal type language used by the current inferencer.
data ExpressionType
  = TIntType
  | TIntegerLiteralType IntegerLiteralRange
  | TFloatType
  | TNumericType NumericType
  | TBoolType
  | TListType ExpressionType
  | TTupleType [ExpressionType]
  | TDataType Identifier [ExpressionType]
  | TFunctionType ExpressionType ExpressionType
  | TVarType Int
  deriving (Eq, Show)

data ConstructorArgumentType
  = ConstructorArgumentMonomorphic ExpressionType
  | ConstructorArgumentParameter Text
  | ConstructorArgumentFresh
  deriving (Eq, Show)

data IntegerLiteralRange = IntegerLiteralRange Integer Integer
  deriving (Eq, Show)

data NumericConstraint
  = AnyNumericConstraint
  | RuntimeArithmeticNumericConstraint
  | RuntimeComparisonNumericConstraint
  | IntegralNumericConstraint
  | IntegralLiteralNumericConstraint IntegerLiteralRange
  deriving (Eq, Show)

data TypeBinding
  = PlainTypeBinding ExpressionType
  | SchemeTypeBinding TypeScheme
  | BuiltinAliasTypeBinding BuiltinSymbol
  | BuiltinOperatorAliasTypeBinding Text
  | OperatorAliasSchemeTypeBinding Text TypeScheme
  | ConstructorTypeBinding Identifier [Identifier] [ConstructorArgumentType]
  deriving (Eq, Show)

data TypeScheme = TypeScheme (Set Int) [Int] [TypeSchemeConstraint] [TypeSchemePrimitiveConstraint] ScopeCapabilityFacts ExpressionType
  deriving (Eq, Show)

data TypeSchemePrimitiveConstraint
  = TypeSchemeNumericConstraint NumericConstraint ExpressionType
  | TypeSchemeStrictEqualityConstraint ExpressionType
  deriving (Eq, Show)

data TypeSchemeConstraint
  = TypeSchemeConstraint Text ExpressionType
  | TypeSchemeInferredConstraint Text ExpressionType
  | TypeSchemeMethodConstraint Text Text ExpressionType
  deriving (Eq, Show)

type TypeEnv = Map Text TypeBinding

data DataTypeBinding = DataTypeBinding [Identifier] [[ConstructorArgumentType]]
  deriving (Eq, Show)

data ClassMethodType = ClassMethodType Text SignaturePayload
  deriving (Eq, Show)

data ImplMethodType = ImplMethodType ConstraintSignatureType
  deriving (Eq, Show)

-- | Mutable inference state threaded explicitly through the checker.
data InferState = InferState
  { inferNextTypeVar :: Int,
    inferSubst :: Map Int ExpressionType,
    -- Type variables originating from strict-equality sections must eventually
    -- resolve to runtime-supported equality families.
    inferStrictEqualityVars :: Set Int,
    -- Type variables originating from generic numeric operators must resolve
    -- to a concrete numeric family before they can be applied.
    inferNumericVars :: Map Int NumericConstraint,
    inferDataTypes :: Map Text DataTypeBinding,
    inferClassFacts :: Map Text Int,
    inferConcreteImplFacts :: Set Text,
    inferClassMethodSignatures :: Map Text ClassMethodType,
    inferConcreteImplMethods :: Map Text [ImplMethodType],
    inferCurrentModulePath :: Maybe [Text],
    inferCurrentModuleLocalCapabilityFacts :: ScopeCapabilityFacts,
    inferModuleCapabilityFacts :: Map [Text] ScopeCapabilityFacts,
    inferRuntimeTypeHints :: Map BindingRuntimeHintKey ConstraintSignatureType,
    inferDeferredExplicitConstraints :: [DeferredExplicitConstraint],
    inferInferredClassConstraints :: [TypeSchemeConstraint],
    inferErrorsRev :: [Diagnostic],
    inferErrorCount :: Int
  }

data DeferredExplicitConstraint = DeferredExplicitConstraint Text (Maybe Text) Bool ExpressionType ScopeCapabilityFacts ScopeCapabilityFacts

initialInferState :: InferState
initialInferState =
  InferState
    { inferNextTypeVar = 0,
      inferSubst = Map.empty,
      inferStrictEqualityVars = Set.empty,
      inferNumericVars = Map.empty,
      inferDataTypes = Map.empty,
      inferClassFacts = Map.empty,
      inferConcreteImplFacts = Set.empty,
      inferClassMethodSignatures = Map.empty,
      inferConcreteImplMethods = Map.empty,
      inferCurrentModulePath = Nothing,
      inferCurrentModuleLocalCapabilityFacts = emptyScopeCapabilityFacts,
      inferModuleCapabilityFacts = Map.empty,
      inferRuntimeTypeHints = Map.empty,
      inferDeferredExplicitConstraints = [],
      inferInferredClassConstraints = [],
      inferErrorsRev = [],
      inferErrorCount = 0
    }

collectExprTypeErrors :: BuiltinResolutionMode -> Expr -> [Diagnostic]
collectExprTypeErrors builtinMode expr =
  fst (collectExprTypeInfo builtinMode expr)

collectExprTypeInfo :: BuiltinResolutionMode -> Expr -> ([Diagnostic], Map BindingRuntimeHintKey ConstraintSignatureType)
collectExprTypeInfo builtinMode expr =
  let (_, finalState) =
        inferExprType
          builtinMode
          Map.empty
          initialInferState
          expr
   in (reverse (inferErrorsRev finalState), inferRuntimeTypeHints finalState)

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
    ELit literal -> (Just (literalExpressionType literal), checkLiteralType state literal)
    EVar name ->
      case Map.lookup nameText env of
        Just localType -> instantiateTypeBinding localType state
        Nothing ->
          case instantiateBuiltinType builtinMode nameText state of
            Just (builtinType, nextState) -> (Just builtinType, nextState)
            Nothing ->
              case instantiateQualifiedMethodType nameText state of
                Just qualifiedMethodResult -> qualifiedMethodResult
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
        Nothing
          | isBuiltinOperatorSymbol operatorSymbol ->
              ( Nothing,
                addTypeError state (mkUnsupportedOperatorValueError operatorSymbol)
              )
        Nothing -> instantiateDeclaredOperatorBindingType env operatorSymbol state
    EList elements -> inferListType builtinMode env state elements
    ETuple elements -> inferTupleType builtinMode env state elements
    EApply functionExpr argumentExpr ->
      case qualifiedMethodApplicationSpine expr state of
        Just (methodKey, argumentExprs)
          | Map.notMember methodKey env ->
              inferQualifiedMethodApplication builtinMode env state methodKey argumentExprs
        Nothing ->
          inferBuiltinOperatorApplyOrGenericApply functionExpr argumentExpr
        _ ->
          inferBuiltinOperatorApplyOrGenericApply functionExpr argumentExpr
    ETypeApplication functionExpr typeArgument ->
      inferExplicitTypeApplication builtinMode env state functionExpr typeArgument
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
                (Just (mergedUnifiedType unifiedState inferredThenType inferredElseType), unifiedState)
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
      case lookupOperatorRule operatorSymbol of
        Just _ ->
          inferBuiltinBinaryOperatorType operatorSymbol leftExpr rightExpr
        Nothing
          | isBuiltinOperatorSymbol operatorSymbol ->
              inferBuiltinBinaryOperatorType operatorSymbol leftExpr rightExpr
        Nothing ->
          inferExprType
            builtinMode
            env
            state
            (EApply (EApply (EOperatorValue operatorSymbol) leftExpr) rightExpr)
    ESectionLeft leftExpr operatorSymbol ->
      case lookupOperatorRule operatorSymbol of
        Just _ ->
          inferBuiltinSectionLeftOperatorType operatorSymbol leftExpr
        Nothing
          | isBuiltinOperatorSymbol operatorSymbol ->
              inferBuiltinSectionLeftOperatorType operatorSymbol leftExpr
        Nothing ->
          inferExprType
            builtinMode
            env
            state
            (EApply (EOperatorValue operatorSymbol) leftExpr)
    ESectionRight operatorSymbol rightExpr ->
      case lookupOperatorRule operatorSymbol of
        Just _ ->
          inferBuiltinSectionRightOperatorType operatorSymbol rightExpr
        Nothing
          | isBuiltinOperatorSymbol operatorSymbol ->
              inferBuiltinSectionRightOperatorType operatorSymbol rightExpr
        Nothing ->
          inferExprType
            builtinMode
            env
            state
            (declaredOperatorRightSectionExpr operatorSymbol rightExpr)
    EBlock statements -> inferScopeType builtinMode env state statements
  where
    inferBuiltinBinaryOperatorType operatorSymbol leftExpr rightExpr =
      let (binaryResult, _, _) =
            inferBuiltinBinaryOperatorTypeWithOperands operatorSymbol leftExpr rightExpr
       in binaryResult

    inferBuiltinBinaryOperatorTypeWithOperands operatorSymbol leftExpr rightExpr =
      let (leftType, stateAfterLeft) =
            inferExprType builtinMode env state leftExpr
          (rightType, stateAfterRight) =
            inferExprType builtinMode env stateAfterLeft rightExpr
       in case (leftType, rightType) of
            (Just inferredLeftType, Just inferredRightType) ->
              ( inferBinaryType
                  operatorSymbol
                  leftExpr
                  rightExpr
                  inferredLeftType
                  inferredRightType
                  stateAfterRight,
                Just inferredLeftType,
                Just inferredRightType
              )
            _ -> ((Nothing, stateAfterRight), leftType, rightType)

    inferBuiltinSectionLeftOperatorType operatorSymbol leftExpr =
      let (leftType, stateAfterLeft) =
            inferExprType builtinMode env state leftExpr
       in case leftType of
            Just inferredLeftType ->
              inferSectionLeftType
                operatorSymbol
                inferredLeftType
                stateAfterLeft
            Nothing -> (Nothing, stateAfterLeft)

    inferBuiltinSectionRightOperatorType operatorSymbol rightExpr =
      let (rightType, stateAfterRight) =
            inferExprType builtinMode env state rightExpr
       in case rightType of
            Just inferredRightType ->
              inferSectionRightType
                operatorSymbol
                inferredRightType
                stateAfterRight
            Nothing -> (Nothing, stateAfterRight)

    inferBuiltinOperatorApplyOrGenericApply functionExpr argumentExpr =
      case builtinOperatorApplicationSpine env expr of
        Just (operatorSymbol, maybeAliasScheme, leftExpr, rightExpr) ->
          let (binaryResult@(maybeBinaryType, stateAfterBinary), maybeLeftType, maybeRightType) =
                inferBuiltinBinaryOperatorTypeWithOperands operatorSymbol leftExpr rightExpr
           in case maybeBinaryType of
                Just _
                  | Just leftType <- maybeLeftType,
                    Just rightType <- maybeRightType ->
                  ( maybeBinaryType,
                    maybe
                      stateAfterBinary
                      (\aliasScheme -> applyOperatorAliasSchemeConstraints operatorSymbol aliasScheme leftType rightType stateAfterBinary)
                      maybeAliasScheme
                  )
                Nothing -> binaryResult
                _ -> binaryResult
        Nothing ->
          inferGenericApplyWithSectionFallback functionExpr argumentExpr

    inferGenericApplyWithSectionFallback functionExpr argumentExpr =
      let genericResult@(maybeGenericType, _) =
            inferGenericApplyType builtinMode env state functionExpr argumentExpr
       in case (maybeGenericType, builtinOperatorSectionApplication expr) of
            (Nothing, Just (operatorSymbol, leftExpr, rightExpr)) ->
              let binaryResult@(maybeBinaryType, _) =
                    inferBuiltinBinaryOperatorType operatorSymbol leftExpr rightExpr
               in case maybeBinaryType of
                    Just _ -> binaryResult
                    Nothing -> genericResult
            _ -> genericResult

inferExprTypeWithExpected ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  ExpressionType ->
  Expr ->
  (Maybe ExpressionType, InferState)
inferExprTypeWithExpected builtinMode env state expectedType expr =
  case (resolveType state expectedType, expr) of
    (TFunctionType argumentType resultType, ELambda parameterName bodyExpr) ->
      let extendedEnv =
            Map.insert
              (identifierText parameterName)
              (PlainTypeBinding argumentType)
              env
          (bodyType, stateAfterBody) =
            inferExprTypeWithExpected builtinMode extendedEnv state resultType bodyExpr
       in
        case bodyType of
          Just inferredBodyType ->
            ( Just
                ( TFunctionType
                    (resolveType stateAfterBody argumentType)
                    inferredBodyType
                ),
              stateAfterBody
            )
          Nothing -> (Nothing, stateAfterBody)
    _ ->
      inferExprType builtinMode env state expr

inferGenericApplyType ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  Expr ->
  (Maybe ExpressionType, InferState)
inferGenericApplyType builtinMode env state functionExpr argumentExpr =
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
              case numericConversionLiteralDiagnostic builtinMode env functionExpr argumentExpr of
                Just diagnostic ->
                  (Nothing, addTypeError unifiedState diagnostic)
                Nothing ->
                  (Just (resolveType unifiedState resultTypeVar), unifiedState)
            Nothing ->
              ( Nothing,
                addTypeError
                  (discardFailedFunctionApplicationConstraints state stateAfterFunction stateWithResultVar)
                  ( mkApplyTypeError
                      (resolveType stateWithResultVar inferredFunctionType)
                      (resolveType stateWithResultVar inferredArgumentType)
                  )
              )
        _ -> (Nothing, stateWithResultVar)

discardFailedFunctionApplicationConstraints :: InferState -> InferState -> InferState -> InferState
discardFailedFunctionApplicationConstraints stateBeforeFunction _ stateAfterApplication =
  stateAfterApplication
    { inferDeferredExplicitConstraints =
        inferDeferredExplicitConstraints stateBeforeFunction
    }

qualifiedMethodApplicationSpine :: Expr -> InferState -> Maybe (Text, [Expr])
qualifiedMethodApplicationSpine expr state =
  case applicationSpine expr of
    Just (methodKey, argumentExprs)
      | qualifiedMethodClassIsVisible methodKey state ->
          Just (methodKey, argumentExprs)
    _ -> Nothing

applicationSpine :: Expr -> Maybe (Text, [Expr])
applicationSpine expr =
  go [] expr
  where
    go argumentExprs currentExpr =
      case currentExpr of
        EApply (EOperatorValue "$") functionExpr ->
          go argumentExprs functionExpr
        EApply functionExpr argumentExpr ->
          go (argumentExpr : argumentExprs) functionExpr
        EVar name ->
          Just (identifierText name, argumentExprs)
        _ ->
          Nothing

builtinOperatorApplicationSpine :: TypeEnv -> Expr -> Maybe (Text, Maybe TypeScheme, Expr, Expr)
builtinOperatorApplicationSpine env expr =
  case dollarAppliedBuiltinOperatorSectionApplication env expr of
    Just (operatorSymbol, leftExpr, rightExpr) ->
      Just (operatorSymbol, Nothing, leftExpr, rightExpr)
    Nothing ->
      case expr of
        EApply (EApply operatorExpr leftExpr) rightExpr -> do
          (operatorSymbol, maybeAliasScheme) <- builtinOperatorSymbolExpr env operatorExpr
          case lookupOperatorRule operatorSymbol of
            Just _ -> Just (operatorSymbol, maybeAliasScheme, leftExpr, rightExpr)
            Nothing -> Nothing
        _ -> Nothing

dollarAppliedBuiltinOperatorSectionApplication :: TypeEnv -> Expr -> Maybe (Text, Expr, Expr)
dollarAppliedBuiltinOperatorSectionApplication env expr =
  case expr of
    EApply (EApply dollarExpr sectionExpr) argumentExpr
      | builtinDollarOperatorExpr env dollarExpr ->
          builtinOperatorSectionApplication (EApply sectionExpr argumentExpr)
    _ -> Nothing

builtinOperatorSectionApplication :: Expr -> Maybe (Text, Expr, Expr)
builtinOperatorSectionApplication expr =
  case expr of
    EApply (ESectionLeft leftExpr operatorSymbol) rightExpr
      | builtinSectionOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, leftExpr, rightExpr)
    EApply (ESectionRight operatorSymbol rightExpr) leftExpr
      | builtinSectionOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, leftExpr, rightExpr)
    _ -> Nothing

builtinSectionOperatorSymbol :: Text -> Bool
builtinSectionOperatorSymbol operatorSymbol =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule _) -> True
    Just StrictEqualityRule -> True
    _ -> False

builtinOperatorSymbolExpr :: TypeEnv -> Expr -> Maybe (Text, Maybe TypeScheme)
builtinOperatorSymbolExpr env expr =
  case expr of
    EOperatorValue operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol ->
          Just (operatorSymbol, Nothing)
    EApply dollarExpr operatorExpr
      | builtinDollarOperatorExpr env dollarExpr ->
          builtinOperatorSymbolExpr env operatorExpr
    EVar name ->
      case Map.lookup (identifierText name) env of
        Just (BuiltinOperatorAliasTypeBinding operatorSymbol) -> Just (operatorSymbol, Nothing)
        Just (OperatorAliasSchemeTypeBinding operatorSymbol typeScheme) -> Just (operatorSymbol, Just typeScheme)
        _ -> Nothing
    _ -> Nothing

builtinDollarOperatorExpr :: TypeEnv -> Expr -> Bool
builtinDollarOperatorExpr env expr =
  case expr of
    EOperatorValue "$" -> True
    EVar name ->
      case Map.lookup (identifierText name) env of
        Just (BuiltinOperatorAliasTypeBinding "$") -> True
        Just (OperatorAliasSchemeTypeBinding "$" _) -> True
        _ -> False
    _ -> False

builtinOperatorAliasSymbol :: Text -> Bool
builtinOperatorAliasSymbol operatorSymbol =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule _) -> True
    Just StrictEqualityRule -> True
    Just ApplicationRule -> True
    _ -> False

applyOperatorAliasSchemeConstraints :: Text -> TypeScheme -> ExpressionType -> ExpressionType -> InferState -> InferState
applyOperatorAliasSchemeConstraints operatorSymbol typeScheme leftType rightType state =
  case lookupOperatorRule operatorSymbol of
    Just StrictEqualityRule ->
      case operatorAliasEqualityConstraintTarget state leftType rightType of
        Just targetType -> instantiateOperatorAliasSchemeConstraints typeScheme targetType state
        Nothing -> state
    Just (NumericRule _) ->
      -- Numeric operator alias schemes only carry the primitive numeric operand
      -- constraint that inferBinaryType has already applied here. User-written
      -- constrained signatures are stored as ordinary schemes, not operator
      -- aliases, so there are no explicit capability facts to replay.
      state
    _ -> state

operatorAliasEqualityConstraintTarget :: InferState -> ExpressionType -> ExpressionType -> Maybe ExpressionType
operatorAliasEqualityConstraintTarget state leftType rightType
  | isJust (typedIntegerFloat64PromotionOperand state leftType rightType) = Nothing
  | resolvedLeftType == resolvedRightType,
    not (structuralRuntimeEqualityType state resolvedLeftType) =
      Just resolvedLeftType
  | otherwise = Nothing
  where
    resolvedLeftType = defaultLiteralTypes (resolveType state leftType)
    resolvedRightType = defaultLiteralTypes (resolveType state rightType)

instantiateOperatorAliasSchemeConstraints :: TypeScheme -> ExpressionType -> InferState -> InferState
instantiateOperatorAliasSchemeConstraints (TypeScheme quantifiedVariables _ explicitConstraints primitiveConstraints definingFacts _) targetType state =
  let replacements =
        Map.fromList
          [ (typeVar, targetType)
            | typeVar <- Set.toList quantifiedVariables
          ]
      instantiatedConstraints =
        map (instantiateTypeSchemeConstraint replacements) explicitConstraints
      instantiatedPrimitiveConstraints =
        map (instantiateTypeSchemePrimitiveConstraint replacements) primitiveConstraints
      stateWithPrimitiveConstraints =
        applyTypeSchemePrimitiveConstraints instantiatedPrimitiveConstraints state
   in deferExplicitConstraintsWithFacts
        (mergeCapabilityFacts definingFacts (capabilityFactsFromState state))
        definingFacts
        instantiatedConstraints
        stateWithPrimitiveConstraints

qualifiedMethodClassIsVisible :: Text -> InferState -> Bool
qualifiedMethodClassIsVisible methodKey state =
  case splitQualifiedMethodKey methodKey of
    Just (capabilityName, _) -> Map.member capabilityName (inferClassFacts state)
    Nothing -> False

inferQualifiedMethodApplication ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Text ->
  [Expr] ->
  (Maybe ExpressionType, InferState)
inferQualifiedMethodApplication builtinMode env state methodKey argumentExprs =
  let (argumentTypes, stateAfterArguments) =
        inferQualifiedMethodArguments builtinMode env state argumentExprs
   in case sequence argumentTypes of
        Nothing -> (Nothing, stateAfterArguments)
        Just typedArgumentTypes ->
          resolveQualifiedMethodApplicationType
            methodKey
            env
            stateAfterArguments
            (zip argumentExprs typedArgumentTypes)

inferQualifiedMethodArguments ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  [Expr] ->
  ([Maybe ExpressionType], InferState)
inferQualifiedMethodArguments builtinMode env state argumentExprs =
  let (reversedTypes, finalState) = foldl' step ([], state) argumentExprs
   in (reverse reversedTypes, finalState)
  where
    step (typesAcc, stateAcc) argumentExpr =
      let (argumentType, stateAfterArgument) =
            inferExprType builtinMode env stateAcc argumentExpr
       in (argumentType : typesAcc, stateAfterArgument)

literalExpressionType :: Literal -> ExpressionType
literalExpressionType literal =
  case literal of
    LInt value -> TIntegerLiteralType (singletonIntegerLiteralRange value)
    LFloat _ _ maybeTargetType ->
      case maybeTargetType of
        Just targetType -> TNumericType targetType
        Nothing -> TFloatType
    LBool _ -> TBoolType

checkLiteralType :: InferState -> Literal -> InferState
checkLiteralType state literal =
  case literal of
    LFloat literalValue literalSource (Just targetType) ->
      case targetedFloatLiteralDiagnostic targetType literalValue literalSource of
        Just diagnostic -> addTypeError state diagnostic
        Nothing -> state
    _ -> state

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
                  ( Just
                      ( mergeIntegerLiteralRanges
                          (resolveType unifiedState inferredExpectedType)
                          (resolveType unifiedState inferredActualType)
                      ),
                    unifiedState
                  )
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

inferTupleType ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  [Expr] ->
  (Maybe ExpressionType, InferState)
inferTupleType builtinMode env state elements =
  go (Just []) state elements
  where
    go maybeReversedTypes stateAcc remainingElements =
      case remainingElements of
        [] ->
          (TTupleType . reverse <$> maybeReversedTypes, stateAcc)
        element : rest ->
          let (elementType, stateAfterElement) =
                inferExprType builtinMode env stateAcc element
              nextReversedTypes =
                case (maybeReversedTypes, elementType) of
                  (Just reversedTypes, Just inferredElementType) ->
                    Just (resolveType stateAfterElement inferredElementType : reversedTypes)
                  _ -> Nothing
           in go nextReversedTypes stateAfterElement rest

data OperatorRule
  = NumericRule NumericRuleResult
  | StrictEqualityRule
  | ApplicationRule

data NumericRuleResult
  = NumericSameTypeResult
  | NumericBoolResult

lookupOperatorRule :: Text -> Maybe OperatorRule
lookupOperatorRule operatorSymbol =
  case operatorSymbol of
    "+" -> Just (NumericRule NumericSameTypeResult)
    "-" -> Just (NumericRule NumericSameTypeResult)
    "*" -> Just (NumericRule NumericSameTypeResult)
    "/" -> Just (NumericRule NumericSameTypeResult)
    "<" -> Just (NumericRule NumericBoolResult)
    "<=" -> Just (NumericRule NumericBoolResult)
    ">" -> Just (NumericRule NumericBoolResult)
    ">=" -> Just (NumericRule NumericBoolResult)
    "==" -> Just StrictEqualityRule
    "!=" -> Just StrictEqualityRule
    "$" -> Just ApplicationRule
    _ -> Nothing

inferBinaryType ::
  Text ->
  Expr ->
  Expr ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
inferBinaryType operatorSymbol leftExpr rightExpr leftType rightType state =
  case lookupOperatorRule operatorSymbol of
    Just (NumericRule resultType) ->
      applyNumericBinaryRule operatorSymbol resultType leftExpr rightExpr leftType rightType state
    Just StrictEqualityRule ->
      applyStrictEqualityBinaryRule operatorSymbol leftExpr rightExpr leftType rightType state
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
  NumericRuleResult ->
  Expr ->
  Expr ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyNumericBinaryRule operatorSymbol resultRule leftExpr rightExpr leftType rightType state =
  case directIntegerFloat64NumericOperand resultRule state leftExpr rightExpr leftType rightType of
    Just (resolvedOperandType, stateAfterFloat64LiteralOperand) ->
      constrainNumericOperand resolvedOperandType stateAfterFloat64LiteralOperand
    Nothing ->
      case unifyTypes leftType rightType state of
        Just stateAfterUnify ->
          let resolvedOperandType = numericBinaryOperandType operatorSymbol resultRule stateAfterUnify leftType rightType
           in constrainNumericOperand resolvedOperandType stateAfterUnify
        Nothing -> numericOperandError state
  where
    constrainNumericOperand resolvedOperandType operandState =
      case constrainNumericOperatorType (numericRuleConstraint resultRule) resolvedOperandType operandState of
        Just stateAfterNumericConstraint ->
          (Just (numericRuleResultType resultRule resolvedOperandType), stateAfterNumericConstraint)
        Nothing ->
          numericOperandError operandState
    numericOperandError errState =
      ( Nothing,
        addTypeError
          errState
          ( mkNumericBinaryTypeError
              operatorSymbol
              resultRule
              (resolveType errState leftType)
              (resolveType errState rightType)
        )
      )

directIntegerFloat64NumericOperand :: NumericRuleResult -> InferState -> Expr -> Expr -> ExpressionType -> ExpressionType -> Maybe (ExpressionType, InferState)
directIntegerFloat64NumericOperand _resultRule state leftExpr rightExpr leftType rightType =
  integerLiteralFloat64PromotionOperand state leftExpr rightExpr leftType rightType
    <|> case typedIntegerFloat64PromotionOperand state leftType rightType of
      Just promotedType -> Just (promotedType, state)
      Nothing -> Nothing

numericRuleResultType :: NumericRuleResult -> ExpressionType -> ExpressionType
numericRuleResultType resultRule operandType =
  case resultRule of
    NumericSameTypeResult -> operandType
    NumericBoolResult -> TBoolType

numericRuleConstraint :: NumericRuleResult -> NumericConstraint
numericRuleConstraint resultRule =
  case resultRule of
    NumericSameTypeResult -> RuntimeArithmeticNumericConstraint
    NumericBoolResult -> RuntimeComparisonNumericConstraint

integerLiteralFloat64PromotionOperand :: InferState -> Expr -> Expr -> ExpressionType -> ExpressionType -> Maybe (ExpressionType, InferState)
integerLiteralFloat64PromotionOperand state leftExpr rightExpr leftType rightType =
  case (resolveType state leftType, resolveType state rightType) of
    (TIntegerLiteralType literalRange, floatType)
      | exprIsIntegerLiteral leftExpr,
        integerLiteralRangeFitsFloat64 literalRange,
        expressionTypeIsFloat64Domain floatType ->
          Just (floatType, state)
    (floatType, TIntegerLiteralType literalRange)
      | exprIsIntegerLiteral rightExpr,
        integerLiteralRangeFitsFloat64 literalRange,
        expressionTypeIsFloat64Domain floatType ->
          Just (floatType, state)
    _ -> Nothing

exprIsIntegerLiteral :: Expr -> Bool
exprIsIntegerLiteral expr =
  case expr of
    ELit (LInt _) -> True
    _ -> False

expressionTypeIsFloat64Domain :: ExpressionType -> Bool
expressionTypeIsFloat64Domain expressionType =
  case expressionType of
    TFloatType -> True
    TNumericType NumericFloat64 -> True
    _ -> False

expressionTypeIsConcreteIntegral :: ExpressionType -> Bool
expressionTypeIsConcreteIntegral expressionType =
  case expressionType of
    TIntType -> True
    TNumericType numericType -> numericTypeIsIntegral numericType
    _ -> False

typedIntegerFloat64PromotionOperand :: InferState -> ExpressionType -> ExpressionType -> Maybe ExpressionType
typedIntegerFloat64PromotionOperand state leftType rightType =
  case (resolveType state leftType, resolveType state rightType) of
    (integralType, floatType)
      | expressionTypeIsConcreteIntegral integralType,
        expressionTypeIsFloat64Domain floatType ->
          Just floatType
    (floatType, integralType)
      | expressionTypeIsFloat64Domain floatType,
        expressionTypeIsConcreteIntegral integralType ->
          Just floatType
    _ -> Nothing

integerLiteralRangeFitsFloat64 :: IntegerLiteralRange -> Bool
integerLiteralRangeFitsFloat64 literalRange =
  case numericTypeFloatIntegerBounds NumericFloat64 of
    Just (lowerBound, upperBound) ->
      let (literalMin, literalMax) = integerLiteralRangeBounds literalRange
       in literalMin >= lowerBound && literalMax <= upperBound
    Nothing -> False

numericBinaryOperandType ::
  Text ->
  NumericRuleResult ->
  InferState ->
  ExpressionType ->
  ExpressionType ->
  ExpressionType
numericBinaryOperandType operatorSymbol resultRule state leftType rightType =
  case (resolveType state leftType, resolveType state rightType) of
    (TIntegerLiteralType leftRange, TIntegerLiteralType rightRange) ->
      TIntegerLiteralType (numericLiteralBinaryRange operatorSymbol resultRule leftRange rightRange)
    (TIntegerLiteralType literalRange, numericType@(TNumericType concreteNumericType))
      | integerLiteralRangeFitsNumericType literalRange concreteNumericType -> numericType
    (numericType@(TNumericType concreteNumericType), TIntegerLiteralType literalRange)
      | integerLiteralRangeFitsNumericType literalRange concreteNumericType -> numericType
    (TIntegerLiteralType {}, TIntType) -> TIntType
    (TIntType, TIntegerLiteralType {}) -> TIntType
    (resolvedLeftType, _) -> resolvedLeftType

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
  Expr ->
  Expr ->
  ExpressionType ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyStrictEqualityBinaryRule operatorSymbol leftExpr rightExpr leftType rightType state =
  case integerLiteralFloat64PromotionOperand state leftExpr rightExpr leftType rightType of
    Just _ ->
      (Just TBoolType, state)
    Nothing ->
      case typedIntegerFloat64PromotionOperand state leftType rightType of
        Just _ ->
          (Just TBoolType, state)
        Nothing ->
          strictEqualityFallback
  where
    strictEqualityFallback =
      case unifyTypes leftType rightType state of
        Just unifiedState ->
          let resolvedType = resolveType unifiedState leftType
           in
            case resolvedType of
              TVarType typeVar ->
                ( Just TBoolType,
                  addInferredEqualityClassConstraintIfVisible
                    (TVarType typeVar)
                    (addStrictEqualityTypeVarConstraint typeVar unifiedState)
                )
              _
                | supportsRuntimeEqualityType unifiedState resolvedType ->
                    (Just TBoolType, unifiedState)
                | otherwise ->
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
  NumericRuleResult ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyNumericSectionLeftRule operatorSymbol resultRule leftType state =
  let resolvedLeftType = resolveType state leftType
   in case constrainNumericOperatorType (numericRuleConstraint resultRule) resolvedLeftType state of
        Just stateAfterNumericConstraint ->
          let (rightType, stateAfterSectionType) =
                numericSectionCounterpartType resolvedLeftType stateAfterNumericConstraint
           in
            ( Just
                ( TFunctionType
                    rightType
                    (numericRuleResultType resultRule rightType)
                ),
              stateAfterSectionType
            )
        Nothing ->
          ( Nothing,
            addTypeError
              state
              (mkNumericSectionOperandTypeError operatorSymbol resultRule (resolveType state leftType))
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
          addInferredEqualityClassConstraintIfVisible
            resolvedLeftType
            (addStrictEqualityTypeVarConstraint typeVar state)
        )
      _
        | supportsRuntimeEqualityType state resolvedLeftType ->
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
  NumericRuleResult ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyNumericSectionRightRule operatorSymbol resultRule rightType state =
  let resolvedRightType = resolveType state rightType
   in case constrainNumericOperatorType (numericRuleConstraint resultRule) resolvedRightType state of
        Just stateAfterNumericConstraint ->
          let (leftType, stateAfterSectionType) =
                numericSectionCounterpartType resolvedRightType stateAfterNumericConstraint
           in
            ( Just
                ( TFunctionType
                    leftType
                    (numericRuleResultType resultRule leftType)
                ),
              stateAfterSectionType
            )
        Nothing ->
          ( Nothing,
            addTypeError
              state
              (mkNumericSectionOperandTypeError operatorSymbol resultRule (resolveType state rightType))
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
          addInferredEqualityClassConstraintIfVisible
            resolvedRightType
            (addStrictEqualityTypeVarConstraint typeVar state)
        )
      _
        | supportsRuntimeEqualityType state resolvedRightType ->
            (Just (TFunctionType resolvedRightType TBoolType), state)
        | otherwise ->
            ( Nothing,
              addTypeError
                state
                (mkStrictEqualityUnsupportedTypeError operatorSymbol resolvedRightType)
            )

numericSectionCounterpartType :: ExpressionType -> InferState -> (ExpressionType, InferState)
numericSectionCounterpartType sectionOperandType state =
  case sectionOperandType of
    TIntegerLiteralType literalRange ->
      let (typeVar, operandType, stateAfterOperandType) = freshTypeVariable state
       in
        ( operandType,
          addNumericTypeVarConstraint typeVar (IntegralLiteralNumericConstraint literalRange) stateAfterOperandType
        )
    _ -> (sectionOperandType, state)

-- | Scope/type-signature handling for block expressions. This mirrors the
-- statement-order rules enforced by the analyzer while threading inferred types.
inferScopeType :: BuiltinResolutionMode -> TypeEnv -> InferState -> [Statement] -> (Maybe ExpressionType, InferState)
inferScopeType builtinMode initialEnv initialState statements =
  let (scopeType, finalState) =
        go initialEnv Nothing Nothing Map.empty Map.empty initialModuleBaselineFacts stateAfterBindingSeeds indexedStatements
   in (scopeType, restoreCapabilityFacts initialState finalState)
  where
    indexedStatements = zip [0 ..] statements
    recursiveGroupsByStatement =
      inferRecursiveGroupsOrdered
        (Set.union (Map.keysSet initialEnv) (builtinNamesInMode builtinMode))
        indexedStatements
    selfRecursiveFunctionStatements =
      inferSelfRecursiveBindings exprContainsFunctionBranch indexedStatements
    bindingNamesByStatement = collectBindingNames indexedStatements
    signedBindingStatements = collectSignedBindingStatements indexedStatements
    statementsByIndex = Map.fromList indexedStatements
    (bindingSeedsByStatement, seededState) =
      allocateBindingSeeds indexedStatements initialState
    stateAfterBindingSeeds = seededState
    initialModuleBaselineFacts = capabilityFactsFromState initialState

    go env lastExprType pendingSignatureType pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts state remainingStatements =
      case remainingStatements of
        [] -> (lastExprType, state)
        (statementIndex, statement) : rest ->
          case statement of
            SModule _ modulePath ->
              go env lastExprType pendingSignatureType pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts (enterModuleCapabilityScope moduleBaselineFacts modulePath state) rest
            SImport _ modulePath maybeAlias maybeSymbolNames ->
              go env lastExprType pendingSignatureType pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts (importModuleCapabilityFacts modulePath maybeAlias maybeSymbolNames state) rest
            SClass {} ->
              let nextState = seedStatementCapabilityFact state statement
                  nextModuleBaselineFacts =
                    updateRootModuleBaselineFacts moduleBaselineFacts state nextState
               in go env lastExprType Nothing pendingSignaturesByStatement recursiveGroupStartStates nextModuleBaselineFacts nextState rest
            SImpl _ capabilityName arguments methods ->
              let seededState = seedStatementCapabilityFact state statement
                  nextState =
                    checkImplMethodBodies builtinMode env seededState capabilityName arguments methods
                  nextModuleBaselineFacts =
                    updateRootModuleBaselineFacts moduleBaselineFacts state nextState
               in go env lastExprType Nothing pendingSignaturesByStatement recursiveGroupStartStates nextModuleBaselineFacts nextState rest
            SData spanValue typeName typeParameters constructors ->
              let (nextEnv, nextState) =
                    registerDataConstructors spanValue typeName typeParameters constructors env state
               in go nextEnv lastExprType Nothing pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts nextState rest
            SSignature name signatureSpan signaturePayload ->
              let (nextPendingSignature, nextState) =
                    case signaturePayloadToSignatureType signaturePayload signatureState of
                      (Just signatureType, stateAfterSignature) ->
                        ( Just
                            ( PendingSignatureType
                                (identifierText name)
                                signatureSpan
                                (signaturePayloadDeclaredType signatureType)
                                (signaturePayloadExplicitConstraints signatureType)
                                (signaturePayloadVariableOrder signatureType)
                            ),
                          restoreCapabilityFacts state stateAfterSignature
                        )
                      (Nothing, stateAfterSignature) ->
                        ( Nothing,
                          addTypeError
                            (restoreCapabilityFacts state stateAfterSignature)
                            (mkInvalidSignatureTypeError signatureState (identifierText name) signatureSpan signaturePayload)
                        )
                  signatureState = state
               in go env lastExprType nextPendingSignature pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts nextState rest
            SLet name bindingSpan valueExpr ->
              let nameText = identifierText name
                  (envForStatement, stateForStatement) =
                    exposeVisibleRecursiveGroupSchemes statementIndex env state
                  recursiveGroupStartStatesForStatement =
                    rememberRecursiveGroupStart statementIndex stateForStatement recursiveGroupStartStates
                  matchingPendingSignature =
                    case pendingSignatureType of
                      Just pendingSignature
                        | pendingSignatureName pendingSignature == nameText ->
                            Just pendingSignature
                      _ -> Nothing
                  envWithRecursiveBindings =
                    recursiveBindingEnv
                      statementIndex
                      envForStatement
                      recursiveGroupsByStatement
                      bindingNamesByStatement
                      bindingSeedsByStatement
                  envWithBindingSeed =
                    case
                        ( shouldSeedSelfRecursiveFunction statementIndex nameText envForStatement,
                          Map.lookup statementIndex bindingSeedsByStatement
                        ) of
                      (True, Just bindingSeed) ->
                        Map.insert nameText (PlainTypeBinding bindingSeed) envWithRecursiveBindings
                      _ -> envWithRecursiveBindings
                  envWithPendingSignature =
                    case matchingPendingSignature of
                      Just pendingSignature ->
                        Map.insert
                          nameText
                          (PlainTypeBinding (pendingSignatureDeclaredType pendingSignature))
                          envWithBindingSeed
                      Nothing -> envWithBindingSeed
                  maybePreservedSchemeAliasBinding =
                    schemePreservingAliasBinding nameText envWithPendingSignature valueExpr
                  maybeExpectedValueType =
                    pendingSignatureDeclaredType <$> matchingPendingSignature
                  (rawValueType, rawStateAfterValue) =
                    case maybePreservedSchemeAliasBinding of
                      Just (SchemeTypeBinding (TypeScheme _ _ _ _ _ schemeType)) ->
                        (Just schemeType, stateForStatement)
                      Just (OperatorAliasSchemeTypeBinding _ (TypeScheme _ _ _ _ _ schemeType)) ->
                        (Just schemeType, stateForStatement)
                      _ ->
                        case maybeExpectedValueType of
                          Just expectedValueType ->
                            inferExprTypeWithExpected builtinMode envWithPendingSignature stateForStatement expectedValueType valueExpr
                          Nothing ->
                            inferExprType builtinMode envWithPendingSignature stateForStatement valueExpr
                  valueType =
                    targetedFractionalLiteralBindingType
                      nameText
                      matchingPendingSignature
                      valueExpr
                      rawValueType
                  stateAfterTargetedLiteralCheck =
                    case targetedFractionalLiteralDiagnostic nameText matchingPendingSignature valueExpr rawValueType of
                      Just diagnostic -> addTypeError rawStateAfterValue diagnostic
                      Nothing -> rawStateAfterValue
                  stateAfterValue =
                    annotateNewErrorsWithPrimarySpan bindingSpan stateForStatement stateAfterTargetedLiteralCheck
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
                    case (matchingPendingSignature, valueType) of
                      (Just pendingSignature, Just inferredType) ->
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
                  stateAfterExplicitConstraintCheck =
                    finalizeDeferredExplicitConstraintsAt
                      bindingSpan
                      stateForStatement
                      stateAfterSignatureCheck
                  nextBindingType =
                    case matchingPendingSignature of
                      Just pendingSignature ->
                        Just (resolveType stateAfterExplicitConstraintCheck (pendingSignatureDeclaredType pendingSignature))
                      _ ->
                        fmap
                          (defaultBindingLiteralTypes . resolveType stateAfterExplicitConstraintCheck)
                          (Map.lookup statementIndex bindingSeedsByStatement)
                  generalizationEnv =
                    generalizationEnvForStatement statementIndex envForStatement
                  droppedInferredSchemeVariables =
                    case (matchingPendingSignature, nextBindingType) of
                      (Just pendingSignature, Just _)
                        | shouldGeneralizeExplicitSignatureBinding generalizationEnv valueExpr pendingSignature ->
                            explicitBindingSchemeVariables generalizationEnv stateAfterExplicitConstraintCheck pendingSignature
                      (_, Just inferredType)
                        | shouldGeneralizeOrdinaryBinding statementIndex generalizationEnv valueExpr matchingPendingSignature ->
                            ordinaryBindingSchemeVariables generalizationEnv stateAfterExplicitConstraintCheck inferredType
                      _ -> Set.empty
                  stateAfterDroppedInferredMethodCheck =
                    case nextBindingType of
                      Just bindingType ->
                        addUnpreservedInferredMethodConstraintErrors
                          bindingSpan
                          generalizationEnv
                          stateForStatement
                          stateAfterExplicitConstraintCheck
                          bindingType
                          droppedInferredSchemeVariables
                      Nothing -> stateAfterExplicitConstraintCheck
                  stateAfterRuntimeHint =
                    case nextBindingType >>= runtimeHintFromExpressionType stateAfterDroppedInferredMethodCheck of
                      Just runtimeHint ->
                        stateAfterDroppedInferredMethodCheck
                          { inferRuntimeTypeHints =
                              Map.insert
                                (bindingRuntimeHintKeyInModule (inferCurrentModulePath stateAfterDroppedInferredMethodCheck) name bindingSpan)
                                runtimeHint
                                (inferRuntimeTypeHints stateAfterDroppedInferredMethodCheck)
                          }
                      Nothing -> stateAfterDroppedInferredMethodCheck
                  maybeNextBinding =
                    maybePreservedSchemeAliasBinding
                      <|> nextBindingForValue
                        statementIndex
                        nameText
                        envForStatement
                        valueExpr
                        nextBindingType
                        matchingPendingSignature
                        stateAfterRuntimeHint
                  stateAfterCapturedConstraintPrune =
                    case maybeNextBinding of
                      Just binding ->
                        pruneCapturedInferredClassConstraints stateForStatement binding stateAfterRuntimeHint
                      Nothing -> stateAfterRuntimeHint
                  nextPendingSignaturesByStatement =
                    case matchingPendingSignature of
                      Just pendingSignature ->
                        Map.insert statementIndex pendingSignature pendingSignaturesByStatement
                      Nothing -> pendingSignaturesByStatement
                  nextEnvBeforeRecursiveGroupGeneralization =
                    case maybeNextBinding of
                      Just binding -> Map.insert nameText binding env
                      Nothing -> env
                  (nextEnv, stateAfterRecursiveGroupPrune) =
                    generalizeCompletedRecursiveGroup
                      nextPendingSignaturesByStatement
                      statementIndex
                      nextEnvBeforeRecursiveGroupGeneralization
                      recursiveGroupStartStatesForStatement
                      stateAfterCapturedConstraintPrune
               in go nextEnv lastExprType Nothing nextPendingSignaturesByStatement recursiveGroupStartStatesForStatement moduleBaselineFacts stateAfterRecursiveGroupPrune rest
            SExpr exprSpan expr ->
              let (envForStatement, stateForStatement) =
                    exposeVisibleRecursiveGroupSchemes statementIndex env state
                  (exprType, rawStateAfterExpr) = inferExprType builtinMode envForStatement stateForStatement expr
                  stateAfterExpr =
                    annotateNewErrorsWithPrimarySpan exprSpan stateForStatement rawStateAfterExpr
                  stateAfterExplicitConstraintCheck =
                    finalizeDeferredExplicitConstraintsAt
                      exprSpan
                      stateForStatement
                      stateAfterExpr
                  stateAfterDroppedInferredMethodCheck =
                    case exprType of
                      Just resultType ->
                        addUnpreservedInferredMethodConstraintErrors
                          exprSpan
                          envForStatement
                          stateForStatement
                          stateAfterExplicitConstraintCheck
                          resultType
                          Set.empty
                      Nothing -> stateAfterExplicitConstraintCheck
               in go env exprType Nothing pendingSignaturesByStatement recursiveGroupStartStates moduleBaselineFacts stateAfterDroppedInferredMethodCheck rest

    nextBindingForValue ::
      Int ->
      Text ->
      TypeEnv ->
      Expr ->
      Maybe ExpressionType ->
      Maybe PendingSignatureType ->
      InferState ->
      Maybe TypeBinding
    nextBindingForValue statementIndex bindingNameText currentEnv valueExpr maybeInferredType maybePendingSignature state =
      let monomorphicBinding =
            if Set.member statementIndex (Map.keysSet recursiveGroupsByStatement)
              then PlainTypeBinding <$> maybeInferredType
              else ordinaryBindingForValue statementIndex currentEnv valueExpr maybeInferredType maybePendingSignature state
       in case valueExpr of
            EOperatorValue operatorSymbol
              | isNothing maybePendingSignature,
                builtinOperatorAliasSymbol operatorSymbol ->
                  Just (operatorAliasBinding operatorSymbol monomorphicBinding)
            EApply _ _
              | isNothing maybePendingSignature,
                Just (operatorSymbol, maybeAliasScheme) <- builtinOperatorSymbolExpr currentEnv valueExpr ->
                  Just (operatorAliasBinding operatorSymbol (SchemeTypeBinding <$> maybeAliasScheme))
            EVar builtinName ->
              let referencedName = identifierText builtinName
               in case Map.lookup referencedName currentEnv of
                    Just (BuiltinAliasTypeBinding builtinSymbol) ->
                      Just (BuiltinAliasTypeBinding builtinSymbol)
                    Just (BuiltinOperatorAliasTypeBinding operatorSymbol)
                      | isNothing maybePendingSignature ->
                          Just (BuiltinOperatorAliasTypeBinding operatorSymbol)
                    Just binding@(OperatorAliasSchemeTypeBinding _ _)
                      | isNothing maybePendingSignature ->
                          Just binding
                    Just constructorBinding@ConstructorTypeBinding {}
                      | isNothing maybePendingSignature,
                        isSyntheticAliasConstructorBinding bindingNameText referencedName ->
                          Just constructorBinding
                    Just _ ->
                      monomorphicBinding
                    Nothing ->
                      case lookupBuiltinSymbolInMode builtinMode referencedName of
                        Just builtinSymbol -> Just (BuiltinAliasTypeBinding builtinSymbol)
                        Nothing -> monomorphicBinding
            _ -> monomorphicBinding

    schemePreservingAliasBinding :: Text -> TypeEnv -> Expr -> Maybe TypeBinding
    schemePreservingAliasBinding bindingNameText currentEnv valueExpr =
      case valueExpr of
        EVar referencedName
          | isSyntheticModuleSchemeBridge bindingNameText (identifierText referencedName) ->
              case Map.lookup (identifierText referencedName) currentEnv of
                Just binding@(SchemeTypeBinding _) -> Just binding
                Just binding@(OperatorAliasSchemeTypeBinding _ _) -> Just binding
                _ -> Nothing
        _ -> Nothing

    isSyntheticModuleSchemeBridge :: Text -> Text -> Bool
    isSyntheticModuleSchemeBridge bindingNameText referencedNameText =
      Text.isPrefixOf "__module::" bindingNameText
        || Text.isPrefixOf "__module::" referencedNameText

    isSyntheticAliasConstructorBinding bindingNameText referencedName =
      Text.isInfixOf "::" bindingNameText && Text.isPrefixOf "__module::" referencedName

    operatorAliasBinding :: Text -> Maybe TypeBinding -> TypeBinding
    operatorAliasBinding operatorSymbol maybeBinding =
      case maybeBinding of
        Just (SchemeTypeBinding typeScheme) ->
          OperatorAliasSchemeTypeBinding operatorSymbol typeScheme
        Just (OperatorAliasSchemeTypeBinding _ typeScheme) ->
          OperatorAliasSchemeTypeBinding operatorSymbol typeScheme
        _ ->
          BuiltinOperatorAliasTypeBinding operatorSymbol

    ordinaryBindingForValue ::
      Int ->
      TypeEnv ->
      Expr ->
      Maybe ExpressionType ->
      Maybe PendingSignatureType ->
      InferState ->
      Maybe TypeBinding
    ordinaryBindingForValue statementIndex currentEnv valueExpr maybeInferredType maybePendingSignature state =
      case maybeInferredType of
        Just _
          | Just pendingSignature <- maybePendingSignature,
            shouldGeneralizeExplicitSignatureBinding currentEnv valueExpr pendingSignature ->
              Just (generalizedExplicitSignatureBinding currentEnv state pendingSignature)
        Just inferredType
          | shouldGeneralizeOrdinaryBinding statementIndex currentEnv valueExpr maybePendingSignature ->
              Just (generalizedOrdinaryBinding currentEnv state inferredType)
        _ -> PlainTypeBinding <$> maybeInferredType

    shouldGeneralizeExplicitSignatureBinding ::
      TypeEnv ->
      Expr ->
      PendingSignatureType ->
      Bool
    shouldGeneralizeExplicitSignatureBinding currentEnv valueExpr pendingSignature =
      not (null (pendingSignatureExplicitConstraints pendingSignature))
        && not (isDirectConstructorAlias currentEnv valueExpr)

    shouldGeneralizeOrdinaryBinding ::
      Int ->
      TypeEnv ->
      Expr ->
      Maybe PendingSignatureType ->
      Bool
    shouldGeneralizeOrdinaryBinding statementIndex currentEnv valueExpr maybePendingSignature =
      isNothing maybePendingSignature
        && Set.notMember statementIndex signedBindingStatements
        && not (isDirectConstructorAlias currentEnv valueExpr)

    generalizationEnvForStatement :: Int -> TypeEnv -> TypeEnv
    generalizationEnvForStatement statementIndex currentEnv =
      case Map.lookup statementIndex recursiveGroupsByStatement of
        Just groupMembers ->
          foldl' (flip Map.delete) currentEnv (recursiveGroupBindingNames groupMembers)
        Nothing ->
          currentEnv

    recursiveGroupBindingNames :: [Int] -> Set Text
    recursiveGroupBindingNames groupMembers =
      Set.fromList
        [ bindingName
          | memberIndex <- groupMembers,
            Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement]
        ]

    rememberRecursiveGroupStart :: Int -> InferState -> Map Int InferState -> Map Int InferState
    rememberRecursiveGroupStart statementIndex state groupStartStates =
      case Map.lookup statementIndex recursiveGroupsByStatement of
        Just groupMembers
          | not (null groupMembers),
            statementIndex == head groupMembers ->
              Map.insert (head groupMembers) state groupStartStates
        _ -> groupStartStates

    generalizeCompletedRecursiveGroup :: Map Int PendingSignatureType -> Int -> TypeEnv -> Map Int InferState -> InferState -> (TypeEnv, InferState)
    generalizeCompletedRecursiveGroup pendingSignatures statementIndex currentEnv groupStartStates state =
      case Map.lookup statementIndex recursiveGroupsByStatement of
        Just groupMembers
          | not (null groupMembers),
            statementIndex == last groupMembers ->
              let groupBindingNames =
                    Set.fromList
                      [ bindingName
                        | memberIndex <- groupMembers,
                          Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement]
                      ]
                  envOutsideGroup =
                    foldl' (flip Map.delete) currentEnv groupBindingNames
                  nextEnv =
                    foldl'
                      (generalizeRecursiveGroupMember pendingSignatures envOutsideGroup state)
                      currentEnv
                      groupMembers
                  groupStartState =
                    Map.findWithDefault state (head groupMembers) groupStartStates
                  groupBindings =
                    [ binding
                      | memberIndex <- groupMembers,
                        Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement],
                        Just binding <- [Map.lookup bindingName nextEnv]
                    ]
               in
                ( nextEnv,
                  pruneCapturedInferredClassConstraintsForBindings groupStartState groupBindings state
                )
        _ -> (currentEnv, state)

    exposeVisibleRecursiveGroupSchemes :: Int -> TypeEnv -> InferState -> (TypeEnv, InferState)
    exposeVisibleRecursiveGroupSchemes statementIndex currentEnv state =
      foldl' exposeGroup (currentEnv, state) recursiveGroups
      where
        recursiveGroups =
          Set.toList (Set.fromList (Map.elems recursiveGroupsByStatement))

        exposeGroup (envAcc, stateAcc) groupMembers
          | null groupMembers =
              (envAcc, stateAcc)
          | statementIndex `elem` groupMembers =
              (envAcc, stateAcc)
          | statementIndex > last groupMembers =
              (envAcc, stateAcc)
          | any (`Set.member` signedBindingStatements) groupMembers =
              (envAcc, stateAcc)
          | interleavedBindingFeedsLaterGroup statementIndex groupMembers =
              (envAcc, stateAcc)
          | laterGroupMemberDependsOnInterveningBinding statementIndex groupMembers =
              (envAcc, stateAcc)
          | null processedMembers =
              (envAcc, stateAcc)
          | otherwise =
              case previewRecursiveGroupState envAcc stateAcc statementIndex groupMembers of
                Nothing ->
                  (envAcc, stateAcc)
                Just previewState ->
                  let groupBindingNames =
                        Set.fromList
                          [ bindingName
                            | memberIndex <- groupMembers,
                              Just bindingName <- [Map.lookup memberIndex bindingNamesByStatement]
                          ]
                      envOutsideGroup =
                        foldl' (flip Map.delete) envAcc groupBindingNames
                      nextEnv =
                        foldl'
                          (exposeRecursiveGroupMember statementIndex envOutsideGroup previewState)
                          envAcc
                          processedMembers
                   in (nextEnv, previewState)
          where
            processedMembers = filter (< statementIndex) groupMembers

    interleavedBindingFeedsLaterGroup :: Int -> [Int] -> Bool
    interleavedBindingFeedsLaterGroup statementIndex groupMembers =
      case Map.lookup statementIndex statementsByIndex of
        Just (SLet bindingName _ _) ->
          let bindingNameText = identifierText bindingName
           in any
                (laterGroupMemberReferences bindingNameText)
                (filter (> statementIndex) groupMembers)
        _ -> False

    laterGroupMemberReferences :: Text -> Int -> Bool
    laterGroupMemberReferences bindingNameText memberIndex =
      case Map.lookup memberIndex statementsByIndex of
        Just (SLet _ _ valueExpr) ->
          Set.member bindingNameText (freeVarsExprWithBound Set.empty valueExpr)
        _ -> False

    laterGroupMemberDependsOnInterveningBinding :: Int -> [Int] -> Bool
    laterGroupMemberDependsOnInterveningBinding statementIndex groupMembers =
      any memberDependsOnInterveningBinding (filter (> statementIndex) groupMembers)
      where
        groupMemberSet = Set.fromList groupMembers

        memberDependsOnInterveningBinding memberIndex =
          case Map.lookup memberIndex statementsByIndex of
            Just (SLet _ _ valueExpr) ->
              let referencedNames = freeVarsExprWithBound Set.empty valueExpr
               in any
                    (interveningBindingIsReferenced referencedNames memberIndex)
                    (Map.toList bindingNamesByStatement)
            _ -> False

        interveningBindingIsReferenced referencedNames memberIndex (bindingIndex, bindingNameText) =
          bindingIndex > statementIndex
            && bindingIndex < memberIndex
            && Set.notMember bindingIndex groupMemberSet
            && Set.member bindingNameText referencedNames

    previewRecursiveGroupState :: TypeEnv -> InferState -> Int -> [Int] -> Maybe InferState
    previewRecursiveGroupState currentEnv state statementIndex groupMembers =
      let previewState = foldl' previewMember state (filter (> statementIndex) groupMembers)
       in if previewIntroducedDiagnostics state previewState
            then Nothing
            else Just (discardPreviewDiagnostics state previewState)
      where
        previewMember stateAcc memberIndex =
          case Map.lookup memberIndex statementsByIndex of
            Just (SLet bindingName bindingSpan valueExpr) ->
              let nameText = identifierText bindingName
                  envWithRecursiveBindings =
                    recursiveBindingEnv
                      memberIndex
                      currentEnv
                      recursiveGroupsByStatement
                      bindingNamesByStatement
                      bindingSeedsByStatement
                  envWithBindingSeed =
                    case
                        ( shouldSeedSelfRecursiveFunction memberIndex nameText currentEnv,
                          Map.lookup memberIndex bindingSeedsByStatement
                        ) of
                      (True, Just bindingSeed) ->
                        Map.insert nameText (PlainTypeBinding bindingSeed) envWithRecursiveBindings
                      _ -> envWithRecursiveBindings
                  (valueType, rawStateAfterValue) =
                    inferExprType builtinMode envWithBindingSeed stateAcc valueExpr
                  stateAfterValue =
                    annotateNewErrorsWithPrimarySpan bindingSpan stateAcc rawStateAfterValue
               in case (Map.lookup memberIndex bindingSeedsByStatement, valueType) of
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
            _ -> stateAcc

        discardPreviewDiagnostics originalState previewState =
          previewState
            { inferErrorsRev = inferErrorsRev originalState,
              inferRuntimeTypeHints = inferRuntimeTypeHints originalState,
              inferDeferredExplicitConstraints = inferDeferredExplicitConstraints originalState
            }

        previewIntroducedDiagnostics originalState previewState =
          length (inferErrorsRev previewState) /= length (inferErrorsRev originalState)

    shouldSeedSelfRecursiveFunction :: Int -> Text -> TypeEnv -> Bool
    shouldSeedSelfRecursiveFunction statementIndex bindingNameText visibleEnv =
      Set.member statementIndex selfRecursiveFunctionStatements
        && Map.notMember bindingNameText visibleEnv

    exposeRecursiveGroupMember :: Int -> TypeEnv -> InferState -> TypeEnv -> Int -> TypeEnv
    exposeRecursiveGroupMember statementIndex envOutsideGroup state currentEnv memberIndex =
      case Map.lookup memberIndex bindingNamesByStatement of
        Just bindingNameText
          | latestBindingIndexBefore statementIndex bindingNameText == Just memberIndex ->
              generalizeRecursiveGroupMember Map.empty envOutsideGroup state currentEnv memberIndex
        _ -> currentEnv

    latestBindingIndexBefore :: Int -> Text -> Maybe Int
    latestBindingIndexBefore statementIndex bindingNameText =
      foldl' latest Nothing (Map.toList bindingNamesByStatement)
      where
        latest currentLatest (memberIndex, memberName)
          | memberIndex < statementIndex,
            memberName == bindingNameText =
              case currentLatest of
                Just previousIndex
                  | previousIndex > memberIndex -> currentLatest
                _ -> Just memberIndex
          | otherwise = currentLatest

    generalizeRecursiveGroupMember :: Map Int PendingSignatureType -> TypeEnv -> InferState -> TypeEnv -> Int -> TypeEnv
    generalizeRecursiveGroupMember pendingSignatures envOutsideGroup state currentEnv memberIndex =
      case (Map.lookup memberIndex statementsByIndex, Map.lookup memberIndex bindingNamesByStatement) of
        (Just (SLet _ _ valueExpr), Just bindingNameText)
          | Just pendingSignature <- Map.lookup memberIndex pendingSignatures,
            shouldGeneralizeExplicitSignatureBinding envOutsideGroup valueExpr pendingSignature ->
              Map.insert
                bindingNameText
                (generalizedExplicitSignatureBinding envOutsideGroup state pendingSignature)
                currentEnv
        (Just (SLet _ _ valueExpr), Just bindingNameText)
          | shouldGeneralizeOrdinaryBinding memberIndex envOutsideGroup valueExpr Nothing ->
              case Map.lookup memberIndex bindingSeedsByStatement of
                Just bindingSeed ->
                  Map.insert
                    bindingNameText
                    (generalizedOrdinaryBinding envOutsideGroup state bindingSeed)
                    currentEnv
                _ -> currentEnv
        _ -> currentEnv

checkImplMethodBodies ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Identifier ->
  [ConstraintSignatureType] ->
  [ImplMethod] ->
  InferState
checkImplMethodBodies builtinMode env state capabilityName arguments methods =
  case arguments of
    [implTarget]
      | concreteConstraintArgument implTarget,
        not (implMethodNamesHaveDuplicates methods) ->
          let implMethodEnv stateForBindings =
                Map.union env (currentImplMethodBindings implTarget stateForBindings)
              checkMethod stateAcc (ImplMethod methodName methodSpan methodExpr) =
                let methodKey = qualifiedMethodKey capabilityName methodName
                 in case Map.lookup methodKey (inferClassMethodSignatures stateAcc) of
                      Nothing ->
                        addTypeError
                          stateAcc
                          (mkImplMethodMissingClassMethodError methodKey methodSpan)
                      Just classMethodType ->
                        let (maybeExpectedType, stateAfterExpectedType) =
                              qualifiedMethodSignatureType
                                methodKey
                                classMethodType
                                (ImplMethodType implTarget)
                                stateAcc
                         in case maybeExpectedType of
                              Nothing ->
                                stateAfterExpectedType
                              Just expectedType ->
                                let (maybeMethodType, rawStateAfterMethod) =
                                      inferExprTypeWithExpected
                                        builtinMode
                                        (implMethodEnv stateAcc)
                                        stateAfterExpectedType
                                        expectedType
                                        methodExpr
                                    stateAfterMethod =
                                      annotateNewErrorsWithPrimarySpan methodSpan stateAfterExpectedType rawStateAfterMethod
                                    stateAfterMethodCheck =
                                      case maybeMethodType of
                                        Just methodType ->
                                          case unifyTypes expectedType methodType stateAfterMethod of
                                            Just unifiedState -> unifiedState
                                            Nothing ->
                                              addTypeError
                                                stateAfterMethod
                                                ( mkImplMethodTypeMismatchError
                                                    methodKey
                                                    methodSpan
                                                    (resolveType stateAfterMethod expectedType)
                                                    (resolveType stateAfterMethod methodType)
                                                )
                                        Nothing ->
                                          stateAfterMethod
                                 in finalizeDeferredExplicitConstraintsAt
                                      methodSpan
                                      stateAfterExpectedType
                                      stateAfterMethodCheck
           in foldl' checkMethod state methods
    _ -> state
  where
    implMethodNamesHaveDuplicates :: [ImplMethod] -> Bool
    implMethodNamesHaveDuplicates implMethods =
      let methodNames = map (\(ImplMethod methodName _ _) -> identifierText methodName) implMethods
       in length methodNames /= Set.size (Set.fromList methodNames)

    currentImplMethodBindings :: ConstraintSignatureType -> InferState -> TypeEnv
    currentImplMethodBindings implTarget stateForBindings =
      Map.fromList
        [ (methodKey, PlainTypeBinding methodType)
          | ImplMethod methodName _ _ <- methods,
            let methodKey = qualifiedMethodKey capabilityName methodName,
            Just (ClassMethodType classParameter methodSignature) <- [Map.lookup methodKey (inferClassMethodSignatures stateForBindings)],
            Just methodType <- [classMethodPayloadToExpressionType stateForBindings classParameter implTarget methodSignature]
        ]

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

data ScopeCapabilityFacts = ScopeCapabilityFacts
  { scopeClassFacts :: Map Text Int,
    scopeConcreteImplFacts :: Set Text,
    scopeClassMethodSignatures :: Map Text ClassMethodType,
    scopeConcreteImplMethods :: Map Text [ImplMethodType]
  }
  deriving (Eq, Show)

emptyScopeCapabilityFacts :: ScopeCapabilityFacts
emptyScopeCapabilityFacts =
  ScopeCapabilityFacts
    { scopeClassFacts = Map.empty,
      scopeConcreteImplFacts = Set.empty,
      scopeClassMethodSignatures = Map.empty,
      scopeConcreteImplMethods = Map.empty
    }

capabilityFactsFromState :: InferState -> ScopeCapabilityFacts
capabilityFactsFromState state =
  ScopeCapabilityFacts
    { scopeClassFacts = inferClassFacts state,
      scopeConcreteImplFacts = inferConcreteImplFacts state,
      scopeClassMethodSignatures = inferClassMethodSignatures state,
      scopeConcreteImplMethods = inferConcreteImplMethods state
    }

typeSchemeDefiningFactsFromState :: InferState -> [TypeSchemeConstraint] -> ScopeCapabilityFacts
typeSchemeDefiningFactsFromState state schemeConstraints =
  case inferCurrentModulePath state of
    Just _ -> typeSchemeReferencedCapabilityFacts schemeConstraints (capabilityFactsFromState state)
    Nothing -> capabilityFactsFromState state

typeSchemeReferencedCapabilityFacts :: [TypeSchemeConstraint] -> ScopeCapabilityFacts -> ScopeCapabilityFacts
typeSchemeReferencedCapabilityFacts schemeConstraints facts =
  facts
    { scopeClassFacts =
        Map.filterWithKey
          (\className _ -> Set.member className referencedCapabilityNames)
          (scopeClassFacts facts),
      scopeConcreteImplFacts =
        Set.filter
          (\implKey -> Set.member (concreteImplFactClassName implKey) referencedCapabilityNames)
          (scopeConcreteImplFacts facts),
      scopeClassMethodSignatures =
        Map.filterWithKey
          (\methodKey _ -> methodKeyReferencesCapturedCapability methodKey)
          (scopeClassMethodSignatures facts),
      scopeConcreteImplMethods =
        Map.filterWithKey
          (\methodKey _ -> methodKeyReferencesCapturedCapability methodKey)
          (scopeConcreteImplMethods facts)
    }
  where
    referencedCapabilityNames =
      Set.fromList
        [ constraintName
          | schemeConstraint <- schemeConstraints,
            let constraintName = typeSchemeConstraintCapabilityName schemeConstraint
        ]

    methodKeyReferencesCapturedCapability methodKey =
      case splitQualifiedMethodKey methodKey of
        Just (className, _) -> Set.member className referencedCapabilityNames
        Nothing -> False

typeSchemeConstraintCapabilityName :: TypeSchemeConstraint -> Text
typeSchemeConstraintCapabilityName constraint =
  case constraint of
    TypeSchemeConstraint constraintName _ -> constraintName
    TypeSchemeInferredConstraint constraintName _ -> constraintName
    TypeSchemeMethodConstraint constraintName _ _ -> constraintName

applyCapabilityFacts :: ScopeCapabilityFacts -> InferState -> InferState
applyCapabilityFacts facts state =
  state
    { inferClassFacts = scopeClassFacts facts,
      inferConcreteImplFacts = scopeConcreteImplFacts facts,
      inferClassMethodSignatures = scopeClassMethodSignatures facts,
      inferConcreteImplMethods = scopeConcreteImplMethods facts
    }

restoreCapabilityFacts :: InferState -> InferState -> InferState
restoreCapabilityFacts previousState nextState =
  nextState
    { inferClassFacts = inferClassFacts previousState,
      inferConcreteImplFacts = inferConcreteImplFacts previousState,
      inferClassMethodSignatures = inferClassMethodSignatures previousState,
      inferConcreteImplMethods = inferConcreteImplMethods previousState,
      inferCurrentModuleLocalCapabilityFacts = inferCurrentModuleLocalCapabilityFacts previousState
    }

mergeCapabilityFacts :: ScopeCapabilityFacts -> ScopeCapabilityFacts -> ScopeCapabilityFacts
mergeCapabilityFacts leftFacts rightFacts =
  ScopeCapabilityFacts
    { scopeClassFacts = Map.union (scopeClassFacts leftFacts) (scopeClassFacts rightFacts),
      scopeConcreteImplFacts =
        Set.union
          (scopeConcreteImplFacts leftFacts)
          (scopeConcreteImplFacts rightFacts),
      scopeClassMethodSignatures =
        Map.union
          (scopeClassMethodSignatures leftFacts)
          (scopeClassMethodSignatures rightFacts),
      scopeConcreteImplMethods =
        Map.unionWith
          (++)
          (scopeConcreteImplMethods leftFacts)
          (scopeConcreteImplMethods rightFacts)
    }

updateRootModuleBaselineFacts :: ScopeCapabilityFacts -> InferState -> InferState -> ScopeCapabilityFacts
updateRootModuleBaselineFacts moduleBaselineFacts previousState nextState =
  case inferCurrentModulePath previousState of
    Nothing -> capabilityFactsFromState nextState
    Just _ -> moduleBaselineFacts

flushCurrentModuleCapabilityFacts :: InferState -> InferState
flushCurrentModuleCapabilityFacts state =
  case inferCurrentModulePath state of
    Just modulePath ->
      state
        { inferModuleCapabilityFacts =
            Map.insert
              modulePath
              (inferCurrentModuleLocalCapabilityFacts state)
              (inferModuleCapabilityFacts state)
        }
    Nothing -> state

enterModuleCapabilityScope :: ScopeCapabilityFacts -> [Text] -> InferState -> InferState
enterModuleCapabilityScope baselineFacts modulePath state =
  (applyCapabilityFacts baselineFacts (flushCurrentModuleCapabilityFacts state))
    { inferCurrentModulePath = Just modulePath,
      inferCurrentModuleLocalCapabilityFacts = emptyScopeCapabilityFacts
    }

importModuleCapabilityFacts :: [Text] -> Maybe Text -> Maybe [Text] -> InferState -> InferState
importModuleCapabilityFacts modulePath maybeAlias maybeSymbolNames state =
  applyCapabilityFacts
    ( mergeCapabilityFacts
        (capabilityFactsFromState state)
        (filterImportedCapabilityFacts maybeAlias maybeSymbolNames (Map.findWithDefault emptyScopeCapabilityFacts modulePath (inferModuleCapabilityFacts state)))
    )
    state

filterImportedCapabilityFacts :: Maybe Text -> Maybe [Text] -> ScopeCapabilityFacts -> ScopeCapabilityFacts
filterImportedCapabilityFacts maybeAlias maybeSymbolNames facts =
  case maybeAlias of
    Just _ -> emptyScopeCapabilityFacts
    Nothing ->
      case maybeSymbolNames of
        Nothing -> facts
        Just symbolNames ->
          facts
            { scopeClassFacts =
                Map.filterWithKey
                  (\className _ -> Set.member className visibleSymbols)
                  (scopeClassFacts facts),
              scopeConcreteImplFacts =
                Set.filter
                  (\implKey -> Set.member (concreteImplFactClassName implKey) visibleSymbols)
                  (scopeConcreteImplFacts facts),
              scopeClassMethodSignatures =
                Map.filterWithKey
                  (\methodKey _ -> qualifiedMethodClassIsVisible methodKey)
                  (scopeClassMethodSignatures facts),
              scopeConcreteImplMethods =
                Map.filterWithKey
                  (\methodKey _ -> qualifiedMethodClassIsVisible methodKey)
                  (scopeConcreteImplMethods facts)
            }
          where
            visibleSymbols = Set.fromList symbolNames
            qualifiedMethodClassIsVisible methodKey =
              case splitQualifiedMethodKey methodKey of
                Just (className, _) -> Set.member className visibleSymbols
                Nothing -> False

seedStatementCapabilityFact :: InferState -> Statement -> InferState
seedStatementCapabilityFact state statement =
  let facts = seedFacts (capabilityFactsFromState state) (0, statement)
      stateWithVisibleFacts = applyCapabilityFacts facts state
   in case inferCurrentModulePath state of
        Just _ ->
          stateWithVisibleFacts
            { inferCurrentModuleLocalCapabilityFacts =
                seedFacts (inferCurrentModuleLocalCapabilityFacts state) (0, statement)
            }
        Nothing ->
          stateWithVisibleFacts

seedFacts :: ScopeCapabilityFacts -> (Int, Statement) -> ScopeCapabilityFacts
seedFacts facts (_, statement) =
  case statement of
    SClass _ capabilityName parameters methods ->
      seedClassMethodFacts
        capabilityName
        parameters
        methods
        facts {scopeClassFacts = Map.insert (identifierText capabilityName) (length parameters) (scopeClassFacts facts)}
    SImpl _ capabilityName arguments methods ->
      seedImplMethodFacts capabilityName arguments methods $
        case concreteImplFactKey capabilityName arguments of
          Just implFactKey ->
            facts {scopeConcreteImplFacts = Set.insert implFactKey (scopeConcreteImplFacts facts)}
          Nothing ->
            facts
    _ -> facts

seedClassMethodFacts ::
  Identifier ->
  [Identifier] ->
  [ClassMethodSignature] ->
  ScopeCapabilityFacts ->
  ScopeCapabilityFacts
seedClassMethodFacts capabilityName parameters methods facts =
  case parameters of
    [classParameter] ->
      facts
        { scopeClassMethodSignatures =
            foldl'
              insertMethodSignature
              (scopeClassMethodSignatures facts)
              methods
        }
      where
        classParameterText = identifierText classParameter
        insertMethodSignature acc (ClassMethodSignature methodName _ methodSignature) =
          Map.insert
            (qualifiedMethodKey capabilityName methodName)
            (ClassMethodType classParameterText methodSignature)
            acc
    _ -> facts

seedImplMethodFacts ::
  Identifier ->
  [ConstraintSignatureType] ->
  [ImplMethod] ->
  ScopeCapabilityFacts ->
  ScopeCapabilityFacts
seedImplMethodFacts capabilityName arguments methods facts =
  case arguments of
    [implTarget]
      | concreteConstraintArgument implTarget ->
          facts
            { scopeConcreteImplMethods =
                foldl'
                  insertImplMethod
                  (scopeConcreteImplMethods facts)
                  methods
            }
      where
        insertImplMethod acc (ImplMethod methodName _ _) =
          Map.insertWith
            (\newMethods existingMethods -> existingMethods ++ newMethods)
            (qualifiedMethodKey capabilityName methodName)
            [ImplMethodType implTarget]
            acc
    _ -> facts

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
        (\(CaseArm _ _ bodyExpr) -> exprContainsFunctionBranch bodyExpr)
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
            ( \(CaseArm _ _ bodyExpr) ->
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

collectSignedBindingStatements :: [(Int, Statement)] -> Set Int
collectSignedBindingStatements statements =
  case statements of
    (_, SSignature signatureName _ _) : (bindingIndex, SLet bindingName _ _) : rest
      | identifierText signatureName == identifierText bindingName ->
          Set.insert bindingIndex (collectSignedBindingStatements rest)
    _ : rest -> collectSignedBindingStatements rest
    [] -> Set.empty

isDirectConstructorAlias :: TypeEnv -> Expr -> Bool
isDirectConstructorAlias env expr =
  case expr of
    EVar referencedName ->
      case Map.lookup (identifierText referencedName) env of
        Just ConstructorTypeBinding {} -> True
        _ -> False
    _ -> False

generalizedOrdinaryBinding :: TypeEnv -> InferState -> ExpressionType -> TypeBinding
generalizedOrdinaryBinding env state expressionType =
  let resolvedType = defaultBindingLiteralTypes (resolveType state expressionType)
      schemeVariables = ordinaryBindingSchemeVariables env state expressionType
      inferredClassConstraints = typeSchemeInferredClassConstraints state schemeVariables
      primitiveConstraints = typeSchemePrimitiveConstraints state schemeVariables
   in
    if Set.null schemeVariables
        && null inferredClassConstraints
        && null primitiveConstraints
      then PlainTypeBinding resolvedType
      else SchemeTypeBinding (TypeScheme schemeVariables (Set.toList schemeVariables) inferredClassConstraints primitiveConstraints (typeSchemeDefiningFactsFromState state inferredClassConstraints) resolvedType)

ordinaryBindingSchemeVariables :: TypeEnv -> InferState -> ExpressionType -> Set Int
ordinaryBindingSchemeVariables env state expressionType =
  let resolvedType = defaultBindingLiteralTypes (resolveType state expressionType)
      freeVariables = freeTypeVariables resolvedType
      environmentVariables = freeTypeVariablesInEnv state env
      quantifiedVariables = Set.difference freeVariables environmentVariables
   in Set.difference
        quantifiedVariables
        (numericConstrainedTypeVariables state)

addUnpreservedInferredMethodConstraintErrors ::
  SourceSpan ->
  TypeEnv ->
  InferState ->
  InferState ->
  ExpressionType ->
  Set Int ->
  InferState
addUnpreservedInferredMethodConstraintErrors spanValue env statementStartState state statementResultType schemeVariables =
  foldl'
    addUnpreservedClassConstraintError
    ( foldl'
        addUnpreservedMethodError
        (foldl' addUnpreservedConcreteMethodConstraintError state droppedConcreteMethodConstraints)
        droppedAmbiguousMethodKeys
    )
    droppedClassConstraints
  where
    droppedClassConstraints =
      dedupeTypeSchemeConstraints
        [ TypeSchemeInferredConstraint constraintName argumentType
          | TypeSchemeInferredConstraint constraintName argumentType <-
              newInferredClassConstraints statementStartState state,
            not (inferredConstraintTargetPreserved state schemeVariables argumentType),
            not (inferredConstraintTargetStillVisibleInEnv state env argumentType),
            inferredConstraintTargetConcrete state argumentType
              || ( not statementIntroducedErrors
                     && inferredConstraintTargetEscapesResult state statementResultType argumentType
                 )
        ]

    droppedMethodConstraints =
      dedupeTypeSchemeConstraints
        [ TypeSchemeMethodConstraint constraintName methodKey argumentType
          | TypeSchemeMethodConstraint constraintName methodKey argumentType <-
              newInferredClassConstraints statementStartState state,
            not (inferredConstraintTargetPreserved state schemeVariables argumentType),
            not (inferredConstraintTargetStillVisibleInEnv state env argumentType),
            not (concreteInferredMethodConstraintSatisfied state constraintName methodKey argumentType)
        ]

    droppedConcreteMethodConstraints =
      [ methodConstraint
        | methodConstraint@(TypeSchemeMethodConstraint _ _ argumentType) <- droppedMethodConstraints,
          inferredConstraintTargetConcrete state argumentType
      ]

    droppedAmbiguousMethodKeys =
      Set.toList
        ( Set.fromList
            [ methodKey
              | TypeSchemeMethodConstraint _ methodKey argumentType <- droppedMethodConstraints,
                not (inferredConstraintTargetConcrete state argumentType)
            ]
        )

    addUnpreservedMethodError stateAcc methodKey =
      addTypeError
        stateAcc
        (setDiagnosticPrimarySpan spanValue (mkAmbiguousQualifiedMethodBodyError methodKey))

    addUnpreservedConcreteMethodConstraintError stateAcc constraint =
      annotateNewErrorsWithPrimarySpan
        spanValue
        stateAcc
        ( resolveDeferredExplicitConstraint
            stateAcc
            (typeSchemeConstraintToDeferredExplicitConstraint (capabilityFactsFromState state) (capabilityFactsFromState state) constraint)
        )

    addUnpreservedClassConstraintError stateAcc constraint =
      annotateNewErrorsWithPrimarySpan
        spanValue
        stateAcc
        ( resolveDeferredExplicitConstraint
            stateAcc
            (typeSchemeConstraintToDeferredExplicitConstraint (capabilityFactsFromState state) (capabilityFactsFromState state) constraint)
        )

    statementIntroducedErrors =
      inferErrorCount state > inferErrorCount statementStartState

newInferredClassConstraints :: InferState -> InferState -> [TypeSchemeConstraint]
newInferredClassConstraints previousState state =
  take newConstraintCount (inferInferredClassConstraints state)
  where
    previousConstraintCount = length (inferInferredClassConstraints previousState)
    currentConstraintCount = length (inferInferredClassConstraints state)
    newConstraintCount = max 0 (currentConstraintCount - previousConstraintCount)

inferredConstraintTargetPreserved :: InferState -> Set Int -> ExpressionType -> Bool
inferredConstraintTargetPreserved state schemeVariables argumentType =
  let targetType = resolveType state argumentType
      targetVariables = freeTypeVariables targetType
   in not (Set.null targetVariables)
        && targetVariables `Set.isSubsetOf` schemeVariables

inferredConstraintTargetConcrete :: InferState -> ExpressionType -> Bool
inferredConstraintTargetConcrete state argumentType =
  let resolvedArgumentType = defaultLiteralTypes (resolveType state argumentType)
   in Set.null (freeTypeVariables resolvedArgumentType)
        && case expressionTypeToRuntimeHint resolvedArgumentType of
          Just _ -> True
          Nothing -> False

inferredConstraintTargetStillVisibleInEnv :: InferState -> TypeEnv -> ExpressionType -> Bool
inferredConstraintTargetStillVisibleInEnv state env argumentType =
  let targetType = resolveType state argumentType
      targetVariables = freeTypeVariables targetType
      environmentVariables = freeTypeVariablesInEnv state env
   in not (Set.null targetVariables)
        && targetVariables `Set.isSubsetOf` environmentVariables

inferredConstraintTargetEscapesResult :: InferState -> ExpressionType -> ExpressionType -> Bool
inferredConstraintTargetEscapesResult state statementResultType argumentType =
  let targetVariables = freeTypeVariables (resolveType state argumentType)
      resultVariables = freeTypeVariables (resolveType state statementResultType)
   in not (Set.null targetVariables)
        && not (Set.null (Set.intersection targetVariables resultVariables))

concreteInferredMethodConstraintSatisfied :: InferState -> Text -> Text -> ExpressionType -> Bool
concreteInferredMethodConstraintSatisfied state constraintName methodKey argumentType =
  let resolvedArgumentType = resolveType state argumentType
      facts = capabilityFactsFromState state
   in Set.null (freeTypeVariables resolvedArgumentType)
        && concreteInferredMethodConstraintHasUniqueCandidate facts state constraintName methodKey resolvedArgumentType

concreteInferredMethodConstraintHasUniqueCandidate :: ScopeCapabilityFacts -> InferState -> Text -> Text -> ExpressionType -> Bool
concreteInferredMethodConstraintHasUniqueCandidate facts state constraintName methodKey argumentType =
  case satisfyingMethodHints of
    [] -> False
    [_] -> True
    _
      | expressionTypeContainsUncommittedIntegerLiteral argumentType ->
          uniqueExactRuntimeCandidateHint state argumentType satisfyingMethodHints
      | otherwise -> True
  where
    satisfyingMethodHints =
      [ argumentHint
        | argumentHint <- inferredConstraintCandidateRuntimeHints facts state (Just methodKey) argumentType,
          concreteImplFactExists constraintName argumentHint facts,
          concreteImplMethodBodyExists methodKey argumentHint facts
      ]

uniqueExactRuntimeCandidateHint :: InferState -> ExpressionType -> [ConstraintSignatureType] -> Bool
uniqueExactRuntimeCandidateHint state argumentType candidateHints =
  case
      [ candidateHint
        | candidateHint <- candidateHints,
          constraintSignatureTypeExactlyMatchesExpressionType state candidateHint argumentType
      ] of
    [candidateHint] ->
      not (constraintSignatureTypeContainsList candidateHint)
    _ -> False

generalizedExplicitSignatureBinding ::
  TypeEnv ->
  InferState ->
  PendingSignatureType ->
  TypeBinding
generalizedExplicitSignatureBinding env state pendingSignature =
  let resolvedType = resolveType state (pendingSignatureDeclaredType pendingSignature)
      resolvedConstraints =
        map (resolveTypeSchemeConstraint state) (pendingSignatureExplicitConstraints pendingSignature)
      schemeVariables = explicitBindingSchemeVariables env state pendingSignature
      inferredClassConstraints =
        typeSchemeInferredClassConstraints state schemeVariables
      schemeConstraints =
        dedupeTypeSchemeConstraints (resolvedConstraints ++ inferredClassConstraints)
      primitiveConstraints = typeSchemePrimitiveConstraints state schemeVariables
   in if Set.null schemeVariables && null schemeConstraints && null primitiveConstraints
        then PlainTypeBinding resolvedType
        else SchemeTypeBinding (TypeScheme schemeVariables (orderedSchemeVariables (pendingSignatureVariableOrder pendingSignature) schemeVariables) schemeConstraints primitiveConstraints (typeSchemeDefiningFactsFromState state schemeConstraints) resolvedType)

pruneCapturedInferredClassConstraints :: InferState -> TypeBinding -> InferState -> InferState
pruneCapturedInferredClassConstraints statementStartState binding =
  pruneCapturedInferredClassConstraintsForBindings statementStartState [binding]

pruneCapturedInferredClassConstraintsForBindings :: InferState -> [TypeBinding] -> InferState -> InferState
pruneCapturedInferredClassConstraintsForBindings statementStartState bindings state =
  if null capturedConstraints
    then state
    else
      state
        { inferInferredClassConstraints =
            filter
              (not . capturedInScheme . resolveTypeSchemeConstraint state)
              statementConstraints
              ++ priorConstraints
        }
  where
    priorConstraintCount = length (inferInferredClassConstraints statementStartState)
    currentConstraints = inferInferredClassConstraints state
    statementConstraintCount = max 0 (length currentConstraints - priorConstraintCount)
    statementConstraints = take statementConstraintCount currentConstraints
    priorConstraints = drop statementConstraintCount currentConstraints
    capturedConstraints =
      [ resolveTypeSchemeConstraint state constraint
        | binding <- bindings,
          Just (TypeScheme _ _ schemeConstraints _ _ _) <- [typeBindingScheme binding],
          constraint <- schemeConstraints,
          typeSchemeConstraintIsInferred constraint
      ]
    capturedInScheme constraint =
      constraint `elem` capturedConstraints

typeBindingScheme :: TypeBinding -> Maybe TypeScheme
typeBindingScheme binding =
  case binding of
    SchemeTypeBinding typeScheme -> Just typeScheme
    OperatorAliasSchemeTypeBinding _ typeScheme -> Just typeScheme
    _ -> Nothing

typeSchemeConstraintIsInferred :: TypeSchemeConstraint -> Bool
typeSchemeConstraintIsInferred constraint =
  case constraint of
    TypeSchemeInferredConstraint {} -> True
    TypeSchemeMethodConstraint {} -> True
    TypeSchemeConstraint {} -> False

explicitBindingSchemeVariables :: TypeEnv -> InferState -> PendingSignatureType -> Set Int
explicitBindingSchemeVariables env state pendingSignature =
  let resolvedType = resolveType state (pendingSignatureDeclaredType pendingSignature)
      resolvedConstraints =
        map (resolveTypeSchemeConstraint state) (pendingSignatureExplicitConstraints pendingSignature)
      freeVariables =
        Set.union
          (freeTypeVariables resolvedType)
          (freeTypeVariablesInTypeSchemeConstraints resolvedConstraints)
      environmentVariables = freeTypeVariablesInEnv state env
   in Set.difference freeVariables environmentVariables

orderedSchemeVariables :: [Int] -> Set Int -> [Int]
orderedSchemeVariables preferredOrder schemeVariables =
  orderedVariables ++ Set.toList unorderedVariables
  where
    orderedVariables =
      filter (`Set.member` schemeVariables) preferredOrder
    unorderedVariables =
      Set.difference schemeVariables (Set.fromList orderedVariables)

typeSchemePrimitiveConstraints :: InferState -> Set Int -> [TypeSchemePrimitiveConstraint]
typeSchemePrimitiveConstraints state schemeVariables =
  numericConstraints ++ equalityConstraints
  where
    targetTypeFor typeVar =
      let targetType = resolveType state (TVarType typeVar)
          targetVariables = freeTypeVariables targetType
       in if not (Set.null targetVariables) && targetVariables `Set.isSubsetOf` schemeVariables
            then Just targetType
            else Nothing

    numericConstraints =
      [ TypeSchemeNumericConstraint numericConstraint targetType
        | (typeVar, numericConstraint) <- Map.toList (inferNumericVars state),
          Just targetType <- [targetTypeFor typeVar]
      ]

    equalityConstraints =
      [ TypeSchemeStrictEqualityConstraint targetType
        | typeVar <- Set.toList (inferStrictEqualityVars state),
          Just targetType <- [targetTypeFor typeVar]
      ]

numericConstrainedTypeVariables :: InferState -> Set Int
numericConstrainedTypeVariables =
  Map.keysSet . inferNumericVars

typeSchemeInferredClassConstraints :: InferState -> Set Int -> [TypeSchemeConstraint]
typeSchemeInferredClassConstraints state schemeVariables =
  dedupeTypeSchemeConstraints qualifiedMethodConstraints
  where
    qualifiedMethodConstraints =
      [ resolvedConstraint
        | constraint <- reverse (inferInferredClassConstraints state),
          Just resolvedConstraint <- [constraintForScheme constraint]
      ]

    constraintForScheme constraint =
      case constraint of
        TypeSchemeConstraint constraintName argumentType ->
          TypeSchemeConstraint constraintName <$> targetTypeFor argumentType
        TypeSchemeInferredConstraint constraintName argumentType ->
          TypeSchemeInferredConstraint constraintName <$> targetTypeFor argumentType
        TypeSchemeMethodConstraint constraintName methodKey argumentType ->
          TypeSchemeMethodConstraint constraintName methodKey <$> targetTypeFor argumentType

    targetTypeFor argumentType =
      let targetType = resolveType state argumentType
          targetVariables = freeTypeVariables targetType
       in if not (Set.null targetVariables) && targetVariables `Set.isSubsetOf` schemeVariables
            then Just targetType
            else Nothing

dedupeTypeSchemeConstraints :: [TypeSchemeConstraint] -> [TypeSchemeConstraint]
dedupeTypeSchemeConstraints =
  foldr insertIfMissing []
  where
    insertIfMissing constraint constraints
      | constraint `elem` constraints = constraints
      | otherwise = constraint : constraints

resolveTypeSchemeConstraint :: InferState -> TypeSchemeConstraint -> TypeSchemeConstraint
resolveTypeSchemeConstraint state constraint =
  case constraint of
    TypeSchemeConstraint constraintName argumentType ->
      TypeSchemeConstraint constraintName (resolveType state argumentType)
    TypeSchemeInferredConstraint constraintName argumentType ->
      TypeSchemeInferredConstraint constraintName (resolveType state argumentType)
    TypeSchemeMethodConstraint constraintName methodKey argumentType ->
      TypeSchemeMethodConstraint constraintName methodKey (resolveType state argumentType)

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

resolveTypeSchemePrimitiveConstraint :: InferState -> TypeSchemePrimitiveConstraint -> TypeSchemePrimitiveConstraint
resolveTypeSchemePrimitiveConstraint state primitiveConstraint =
  case primitiveConstraint of
    TypeSchemeNumericConstraint numericConstraint argumentType ->
      TypeSchemeNumericConstraint numericConstraint (resolveType state argumentType)
    TypeSchemeStrictEqualityConstraint argumentType ->
      TypeSchemeStrictEqualityConstraint (resolveType state argumentType)

freeTypeVariablesInTypeSchemePrimitiveConstraints :: [TypeSchemePrimitiveConstraint] -> Set Int
freeTypeVariablesInTypeSchemePrimitiveConstraints primitiveConstraints =
  Set.unions (map freeTypeVariablesInTypeSchemePrimitiveConstraint primitiveConstraints)

freeTypeVariablesInTypeSchemePrimitiveConstraint :: TypeSchemePrimitiveConstraint -> Set Int
freeTypeVariablesInTypeSchemePrimitiveConstraint primitiveConstraint =
  case primitiveConstraint of
    TypeSchemeNumericConstraint _ argumentType -> freeTypeVariables argumentType
    TypeSchemeStrictEqualityConstraint argumentType -> freeTypeVariables argumentType

freeTypeVariablesInEnv :: InferState -> TypeEnv -> Set Int
freeTypeVariablesInEnv state =
  Set.unions . map (freeTypeVariablesInBinding state) . Map.elems

freeTypeVariablesInBinding :: InferState -> TypeBinding -> Set Int
freeTypeVariablesInBinding state binding =
  case binding of
    PlainTypeBinding expressionType ->
      freeTypeVariables (resolveType state expressionType)
    SchemeTypeBinding (TypeScheme quantifiedVariables _ explicitConstraints primitiveConstraints _ expressionType) ->
      Set.difference
        ( Set.unions
            [ freeTypeVariables (resolveType state expressionType),
              freeTypeVariablesInTypeSchemeConstraints (map (resolveTypeSchemeConstraint state) explicitConstraints),
              freeTypeVariablesInTypeSchemePrimitiveConstraints (map (resolveTypeSchemePrimitiveConstraint state) primitiveConstraints)
            ]
        )
        quantifiedVariables
    OperatorAliasSchemeTypeBinding _ (TypeScheme quantifiedVariables _ explicitConstraints primitiveConstraints _ expressionType) ->
      Set.difference
        ( Set.unions
            [ freeTypeVariables (resolveType state expressionType),
              freeTypeVariablesInTypeSchemeConstraints (map (resolveTypeSchemeConstraint state) explicitConstraints),
              freeTypeVariablesInTypeSchemePrimitiveConstraints (map (resolveTypeSchemePrimitiveConstraint state) primitiveConstraints)
            ]
        )
        quantifiedVariables
    BuiltinAliasTypeBinding {} -> Set.empty
    BuiltinOperatorAliasTypeBinding {} -> Set.empty
    ConstructorTypeBinding _ _ argumentTypes ->
      Set.unions (map (freeTypeVariablesInConstructorArgument state) argumentTypes)

freeTypeVariablesInConstructorArgument :: InferState -> ConstructorArgumentType -> Set Int
freeTypeVariablesInConstructorArgument state argumentType =
  case argumentType of
    ConstructorArgumentMonomorphic expressionType ->
      freeTypeVariables (resolveType state expressionType)
    ConstructorArgumentParameter {} -> Set.empty
    ConstructorArgumentFresh -> Set.empty

freeTypeVariables :: ExpressionType -> Set Int
freeTypeVariables expressionType =
  case expressionType of
    TIntType -> Set.empty
    TIntegerLiteralType {} -> Set.empty
    TFloatType -> Set.empty
    TNumericType {} -> Set.empty
    TBoolType -> Set.empty
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

-- | Pending type signature state mirrors analyzer adjacency rules while
-- carrying the normalized declaration type for the next binding.
data PendingSignatureType = PendingSignatureType
  { pendingSignatureName :: Text,
    pendingSignatureSpan :: SourceSpan,
    pendingSignatureDeclaredType :: ExpressionType,
    pendingSignatureExplicitConstraints :: [TypeSchemeConstraint],
    pendingSignatureVariableOrder :: [Int]
  }

targetedFractionalLiteralBindingType ::
  Text ->
  Maybe PendingSignatureType ->
  Expr ->
  Maybe ExpressionType ->
  Maybe ExpressionType
targetedFractionalLiteralBindingType bindingName maybePendingSignature valueExpr maybeInferredType =
  case targetedFractionalLiteralType bindingName maybePendingSignature valueExpr maybeInferredType of
    Just targetType -> Just (TNumericType targetType)
    Nothing -> maybeInferredType

targetedFractionalLiteralDiagnostic ::
  Text ->
  Maybe PendingSignatureType ->
  Expr ->
  Maybe ExpressionType ->
  Maybe Diagnostic
targetedFractionalLiteralDiagnostic bindingName maybePendingSignature valueExpr maybeInferredType =
  case (targetedFractionalLiteralType bindingName maybePendingSignature valueExpr maybeInferredType, valueExpr) of
    (Just targetType, ELit (LFloat literalValue literalSource Nothing)) ->
      targetedFloatLiteralDiagnostic targetType literalValue literalSource
    _ -> Nothing

targetedFractionalLiteralType ::
  Text ->
  Maybe PendingSignatureType ->
  Expr ->
  Maybe ExpressionType ->
  Maybe NumericType
targetedFractionalLiteralType bindingName maybePendingSignature valueExpr maybeInferredType =
  case (maybePendingSignature, valueExpr, maybeInferredType) of
    (Just pendingSignature, ELit (LFloat _ _ Nothing), Just TFloatType)
      | pendingSignatureName pendingSignature == bindingName ->
          concreteFloatNumericType (pendingSignatureDeclaredType pendingSignature)
    _ -> Nothing

concreteFloatNumericType :: ExpressionType -> Maybe NumericType
concreteFloatNumericType expressionType =
  case expressionType of
    TNumericType NumericFloat16 -> Just NumericFloat16
    TNumericType NumericFloat32 -> Just NumericFloat32
    TNumericType NumericFloat64 -> Just NumericFloat64
    _ -> Nothing

registerDataConstructors :: SourceSpan -> Identifier -> [Identifier] -> [DataConstructor] -> TypeEnv -> InferState -> (TypeEnv, InferState)
registerDataConstructors spanValue typeName typeParameters constructors env initialState =
  case Map.lookup typeNameText (inferDataTypes initialState) of
    Just _ ->
      ( env,
        addTypeError
          initialState
          (mkDuplicateDataTypeDeclarationError typeNameText spanValue)
      )
    Nothing ->
      let (nextEnv, nextState, constructorPayloadsRev) =
            foldl' register (env, initialState, []) constructors
       in
        ( nextEnv,
          nextState
            { inferDataTypes =
                Map.insert
                  typeNameText
                  (DataTypeBinding typeParameters (reverse constructorPayloadsRev))
                  (inferDataTypes nextState)
            }
        )
  where
    typeNameText = identifierText typeName

    register (envAcc, stateAcc, constructorPayloadsAcc) (DataConstructor constructorName constructorArguments) =
      let (argumentTypes, nextState) =
            constructorArgumentTypes typeParameters constructorArguments stateAcc
       in
        ( Map.insert
            (identifierText constructorName)
            (ConstructorTypeBinding typeName typeParameters argumentTypes)
            envAcc,
          nextState,
          argumentTypes : constructorPayloadsAcc
        )

constructorArgumentTypes :: [Identifier] -> [DataConstructorArgument] -> InferState -> ([ConstructorArgumentType], InferState)
constructorArgumentTypes typeParameters constructorArguments state
  | null typeParameters =
      let (argumentTypes, nextState) = freshTypeVars (length constructorArguments) state
       in (map ConstructorArgumentMonomorphic argumentTypes, nextState)
  | otherwise =
      foldl' collectArgument ([], state) constructorArguments
  where
    typeParameterNames = Set.fromList (map identifierText typeParameters)

    collectArgument (argumentTypes, stateAcc) constructorArgument =
      case constructorArgument of
        DataConstructorArgumentName argumentName
          | Set.member (identifierText argumentName) typeParameterNames ->
              (argumentTypes ++ [ConstructorArgumentParameter (identifierText argumentName)], stateAcc)
          | Just payloadType <- namedConstructorPayloadType argumentName ->
              (argumentTypes ++ [ConstructorArgumentMonomorphic payloadType], stateAcc)
          | otherwise ->
              ( argumentTypes ++ [ConstructorArgumentFresh],
                addTypeError stateAcc (mkUnknownConstructorPayloadTypeError argumentName)
              )
        DataConstructorArgumentOpaque ->
          (argumentTypes ++ [ConstructorArgumentFresh], stateAcc)

namedConstructorPayloadType :: Identifier -> Maybe ExpressionType
namedConstructorPayloadType =
  constraintSignatureTypeToExpressionType . ConstraintTypeName

-- | Instantiate local bindings and constructors at use sites. Constructors are
-- rendered as curried functions ending in their declared data type.
instantiateTypeBinding :: TypeBinding -> InferState -> (Maybe ExpressionType, InferState)
instantiateTypeBinding binding state =
  case binding of
    PlainTypeBinding expressionType ->
      (Just (resolveType state expressionType), state)
    SchemeTypeBinding typeScheme ->
      instantiateTypeScheme typeScheme state
    BuiltinAliasTypeBinding builtinSymbol ->
      case instantiateBuiltinSymbolType builtinSymbol state of
        Just (expressionType, nextState) -> (Just expressionType, nextState)
        Nothing -> (Nothing, state)
    BuiltinOperatorAliasTypeBinding operatorSymbol ->
      case instantiateOperatorType operatorSymbol state of
        Just (operatorType, nextState) -> (Just operatorType, nextState)
        Nothing -> (Nothing, state)
    OperatorAliasSchemeTypeBinding _ typeScheme ->
      instantiateTypeScheme typeScheme state
    ConstructorTypeBinding {} ->
      case instantiateConstructorBinding binding state of
        Just (constructorArgumentTypes', constructorResultType, nextState) ->
          ( Just
              (foldr TFunctionType constructorResultType constructorArgumentTypes'),
            nextState
          )
        Nothing -> (Nothing, state)

instantiateTypeScheme :: TypeScheme -> InferState -> (Maybe ExpressionType, InferState)
instantiateTypeScheme (TypeScheme quantifiedVariables quantifiedOrder explicitConstraints primitiveConstraints definingFacts expressionType) state =
  let (freshBindings, nextState) =
        foldl'
          allocateFreshBinding
          (Map.empty, state)
          (orderedSchemeVariables quantifiedOrder quantifiedVariables)
      instantiatedType =
        replaceTypeVariables freshBindings expressionType
      instantiatedConstraints =
        map (instantiateTypeSchemeConstraint freshBindings) explicitConstraints
      instantiatedPrimitiveConstraints =
        map (instantiateTypeSchemePrimitiveConstraint freshBindings) primitiveConstraints
      stateWithPrimitiveConstraints =
        applyTypeSchemePrimitiveConstraints instantiatedPrimitiveConstraints nextState
      stateWithDeferredConstraints =
        deferExplicitConstraintsWithFacts
          (mergeCapabilityFacts definingFacts (capabilityFactsFromState state))
          definingFacts
          instantiatedConstraints
          stateWithPrimitiveConstraints
   in (Just (resolveType stateWithDeferredConstraints instantiatedType), stateWithDeferredConstraints)
  where
    allocateFreshBinding (bindings, stateAcc) typeVar =
      let (freshType, nextState) = freshTypeVar stateAcc
       in (Map.insert typeVar freshType bindings, nextState)

inferExplicitTypeApplication ::
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  SignatureType ->
  (Maybe ExpressionType, InferState)
inferExplicitTypeApplication builtinMode env state functionExpr typeArgument =
  case explicitTypeApplicationScheme env functionExpr of
    Just typeScheme ->
      instantiateTypeSchemeWithExplicitArgument
        typeScheme
        (signatureTypeToExpressionType typeArgument)
        state
    Nothing ->
      let (maybeFunctionType, stateAfterFunction) =
            inferExprType builtinMode env state functionExpr
       in
        case maybeFunctionType of
          Just _ ->
            (Nothing, addTypeError stateAfterFunction mkExplicitTypeApplicationTargetError)
          Nothing -> (Nothing, stateAfterFunction)

explicitTypeApplicationScheme :: TypeEnv -> Expr -> Maybe TypeScheme
explicitTypeApplicationScheme env functionExpr =
  case functionExpr of
    EVar name ->
      Map.lookup (identifierText name) env >>= typeBindingScheme
    EOperatorValue operatorSymbol ->
      Map.lookup (operatorBindingIdentifierText operatorSymbol) env >>= typeBindingScheme
    _ -> Nothing

instantiateTypeSchemeWithExplicitArgument ::
  TypeScheme ->
  ExpressionType ->
  InferState ->
  (Maybe ExpressionType, InferState)
instantiateTypeSchemeWithExplicitArgument (TypeScheme quantifiedVariables quantifiedOrder explicitConstraints primitiveConstraints definingFacts expressionType) explicitArgumentType state =
  case orderedSchemeVariables quantifiedOrder quantifiedVariables of
    [] ->
      (Nothing, addTypeError state mkExplicitTypeApplicationTargetError)
    explicitTypeVar : remainingTypeVars ->
      let (freshBindings, nextState) =
            foldl'
              allocateFreshBinding
              (Map.singleton explicitTypeVar explicitArgumentType, state)
              remainingTypeVars
          instantiatedType =
            replaceTypeVariables freshBindings expressionType
          instantiatedConstraints =
            map (instantiateTypeSchemeConstraint freshBindings) explicitConstraints
          instantiatedPrimitiveConstraints =
            map (instantiateTypeSchemePrimitiveConstraint freshBindings) primitiveConstraints
          stateWithPrimitiveConstraints =
            applyTypeSchemePrimitiveConstraints instantiatedPrimitiveConstraints nextState
          stateWithDeferredConstraints =
            deferExplicitConstraintsWithFacts
              (mergeCapabilityFacts definingFacts (capabilityFactsFromState state))
              definingFacts
              instantiatedConstraints
              stateWithPrimitiveConstraints
       in (Just (resolveType stateWithDeferredConstraints instantiatedType), stateWithDeferredConstraints)
  where
    allocateFreshBinding (bindings, stateAcc) typeVar =
      let (freshType, nextState) = freshTypeVar stateAcc
       in (Map.insert typeVar freshType bindings, nextState)

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

applyTypeSchemePrimitiveConstraints :: [TypeSchemePrimitiveConstraint] -> InferState -> InferState
applyTypeSchemePrimitiveConstraints primitiveConstraints state =
  foldl' applyPrimitiveConstraint state primitiveConstraints
  where
    applyPrimitiveConstraint stateAcc primitiveConstraint =
      case primitiveConstraint of
        TypeSchemeNumericConstraint numericConstraint argumentType ->
          case constrainNumericOperatorType numericConstraint argumentType stateAcc of
            Just nextState -> nextState
            Nothing ->
              addTypeError
                stateAcc
                (mkTypeSchemeNumericConstraintError numericConstraint (resolveType stateAcc argumentType))
        TypeSchemeStrictEqualityConstraint argumentType ->
          case resolveType stateAcc argumentType of
            TVarType typeVar ->
              addStrictEqualityTypeVarConstraint typeVar stateAcc
            resolvedType
              | supportsRuntimeEqualityType stateAcc resolvedType ->
                  stateAcc
              | otherwise ->
                  addTypeError stateAcc (mkTypeSchemeStrictEqualityConstraintError resolvedType)

deferExplicitConstraints :: [TypeSchemeConstraint] -> InferState -> InferState
deferExplicitConstraints explicitConstraints state =
  deferExplicitConstraintsWithFacts (capabilityFactsFromState state) (capabilityFactsFromState state) explicitConstraints state

deferExplicitConstraintsWithFacts :: ScopeCapabilityFacts -> ScopeCapabilityFacts -> [TypeSchemeConstraint] -> InferState -> InferState
deferExplicitConstraintsWithFacts facts structuralFacts explicitConstraints state
  | null explicitConstraints = state
  | otherwise =
      state
        { inferDeferredExplicitConstraints =
            inferDeferredExplicitConstraints state
              ++ map (typeSchemeConstraintToDeferredExplicitConstraint facts structuralFacts) explicitConstraints
        }

typeSchemeConstraintToDeferredExplicitConstraint :: ScopeCapabilityFacts -> ScopeCapabilityFacts -> TypeSchemeConstraint -> DeferredExplicitConstraint
typeSchemeConstraintToDeferredExplicitConstraint facts structuralFacts constraint =
  case constraint of
    TypeSchemeConstraint constraintName argumentType ->
      DeferredExplicitConstraint constraintName Nothing False argumentType facts structuralFacts
    TypeSchemeInferredConstraint constraintName argumentType ->
      DeferredExplicitConstraint constraintName Nothing True argumentType facts structuralFacts
    TypeSchemeMethodConstraint constraintName methodKey argumentType ->
      DeferredExplicitConstraint constraintName (Just methodKey) True argumentType facts structuralFacts

finalizeDeferredExplicitConstraintsAt :: SourceSpan -> InferState -> InferState -> InferState
finalizeDeferredExplicitConstraintsAt spanValue statementStartState state =
  annotateNewErrorsWithPrimarySpan
    spanValue
    state
    (resolveStatementDeferredExplicitConstraints statementStartState state)

resolveStatementDeferredExplicitConstraints :: InferState -> InferState -> InferState
resolveStatementDeferredExplicitConstraints statementStartState state =
  foldl' resolveDeferredExplicitConstraint stateWithoutStatementConstraints statementConstraints
  where
    priorConstraints = inferDeferredExplicitConstraints statementStartState
    currentConstraints = inferDeferredExplicitConstraints state
    statementConstraints =
      drop (length priorConstraints) currentConstraints
    stateWithoutStatementConstraints =
      state {inferDeferredExplicitConstraints = priorConstraints}

resolveDeferredExplicitConstraint :: InferState -> DeferredExplicitConstraint -> InferState
resolveDeferredExplicitConstraint state (DeferredExplicitConstraint constraintName maybeMethodKey inferredConstraint argumentType facts structuralFacts) =
  let unresolvedArgumentType =
        resolveType state argumentType
      resolvedArgumentType =
        defaultLiteralTypes unresolvedArgumentType
   in
    if not (Set.null (freeTypeVariables unresolvedArgumentType))
      then addTypeError state (mkAmbiguousDeferredConstraintError inferredConstraint constraintName resolvedArgumentType)
      else
        case Map.lookup constraintName (scopeClassFacts facts) of
          Nothing ->
            addTypeError state (mkMissingExplicitConstraintClassError constraintName)
          Just classArity
            | classArity /= 1 ->
                addTypeError state (mkExplicitConstraintArityError constraintName classArity)
            | otherwise ->
                case constraintRuntimeHintsForDeferred facts state inferredConstraint constraintName maybeMethodKey unresolvedArgumentType of
                  [] ->
                    addTypeError state (mkAmbiguousDeferredConstraintError inferredConstraint constraintName resolvedArgumentType)
                  argumentHints ->
                    let implFactHints =
                          filter
                            (constraintImplFactExistsForDeferred facts inferredConstraint constraintName)
                            argumentHints
                        methodBodyHints methodKey =
                          filter
                            (\argumentHint -> concreteImplMethodBodyExists methodKey argumentHint facts)
                            implFactHints
                        ambiguousMethodBodyHints methodKey =
                          inferredConstraint
                            && expressionTypeContainsUncommittedIntegerLiteral unresolvedArgumentType
                            && length (methodBodyHints methodKey) > 1
                            && not (uniqueExactRuntimeCandidateHint state unresolvedArgumentType (methodBodyHints methodKey))
                        renderedImplFactKey =
                          constraintName <> "(" <> renderConstraintSignatureType (head argumentHints) <> ")"
                     in case maybeMethodKey of
                          Nothing
                            | not (null implFactHints) ->
                                state
                            | inferredConstraint
                                && inferredEqualityConstraintCanUseStructuralRuntimeEquality state structuralFacts maybeMethodKey constraintName resolvedArgumentType ->
                                state
                            | otherwise ->
                                addTypeError state (mkMissingExplicitConstraintImplFactError renderedImplFactKey)
                          Just methodKey
                            | null implFactHints ->
                                addTypeError state (mkMissingExplicitConstraintImplFactError renderedImplFactKey)
                            | ambiguousMethodBodyHints methodKey ->
                                addTypeError state (mkAmbiguousQualifiedMethodBodyError methodKey)
                            | not (null (methodBodyHints methodKey)) ->
                                state
                            | otherwise ->
                                addTypeError state (mkMissingImplMethodBodyError methodKey)

expressionTypeContainsUncommittedIntegerLiteral :: ExpressionType -> Bool
expressionTypeContainsUncommittedIntegerLiteral expressionType =
  case expressionType of
    TIntegerLiteralType {} -> True
    TListType elementType ->
      expressionTypeContainsUncommittedIntegerLiteral elementType
    TTupleType elementTypes ->
      any expressionTypeContainsUncommittedIntegerLiteral elementTypes
    TDataType _ typeArguments ->
      any expressionTypeContainsUncommittedIntegerLiteral typeArguments
    TFunctionType argumentType resultType ->
      expressionTypeContainsUncommittedIntegerLiteral argumentType
        || expressionTypeContainsUncommittedIntegerLiteral resultType
    _ -> False

constraintRuntimeHintsForDeferred ::
  ScopeCapabilityFacts ->
  InferState ->
  Bool ->
  Text ->
  Maybe Text ->
  ExpressionType ->
  [ConstraintSignatureType]
constraintRuntimeHintsForDeferred facts state inferredConstraint _ maybeMethodKey argumentType
  | inferredConstraint =
      inferredConstraintCandidateRuntimeHints facts state maybeMethodKey argumentType
  | otherwise =
      case expressionTypeToRuntimeHint (defaultLiteralTypes argumentType) of
        Just argumentHint -> [argumentHint]
        Nothing -> []

constraintImplFactExistsForDeferred :: ScopeCapabilityFacts -> Bool -> Text -> ConstraintSignatureType -> Bool
constraintImplFactExistsForDeferred facts inferredConstraint constraintName argumentHint =
  if inferredConstraint
    then concreteImplFactExists constraintName argumentHint facts
    else Set.member implFactKey (scopeConcreteImplFacts facts)
  where
    implFactKey = constraintName <> "(" <> renderConstraintSignatureType argumentHint <> ")"

inferredConstraintCandidateRuntimeHints :: ScopeCapabilityFacts -> InferState -> Maybe Text -> ExpressionType -> [ConstraintSignatureType]
inferredConstraintCandidateRuntimeHints facts state maybeMethodKey argumentType =
  dedupeConstraintSignatureTypes (defaultHint ++ methodCandidateHints)
  where
    defaultHint =
      case expressionTypeToRuntimeHint (defaultLiteralTypes argumentType) of
        Just argumentHint -> [argumentHint]
        Nothing -> []

    methodCandidateHints =
      case maybeMethodKey of
        Nothing -> []
        Just methodKey ->
          [ implTarget
            | ImplMethodType implTarget <- Map.findWithDefault [] methodKey (scopeConcreteImplMethods facts),
              constraintSignatureTypeMatchesExpressionType state implTarget argumentType
          ]

dedupeConstraintSignatureTypes :: [ConstraintSignatureType] -> [ConstraintSignatureType]
dedupeConstraintSignatureTypes =
  go Set.empty
  where
    go _ [] = []
    go seen (signatureType : rest)
      | Set.member rendered seen = go seen rest
      | otherwise = signatureType : go (Set.insert rendered seen) rest
      where
        rendered = renderConstraintSignatureType signatureType

constraintSignatureTypeMatchesExpressionType :: InferState -> ConstraintSignatureType -> ExpressionType -> Bool
constraintSignatureTypeMatchesExpressionType state signatureType expressionType =
  case (signatureType, resolveType state expressionType) of
    (ConstraintTypeName signatureName, TIntegerLiteralType literalRange) ->
      case numericTypeFromConstraintSignatureName (identifierText signatureName) of
        Just numericType ->
          numericTypeIsIntegral numericType
            && integerLiteralRangeFitsNumericType literalRange numericType
        Nothing ->
          False
    (ConstraintTypeList signatureElementType, TListType elementType) ->
      constraintSignatureTypeMatchesExpressionType state signatureElementType elementType
    (ConstraintTypeTuple signatureElementTypes, TTupleType elementTypes)
      | length signatureElementTypes == length elementTypes ->
          and (zipWith (constraintSignatureTypeMatchesExpressionType state) signatureElementTypes elementTypes)
    (ConstraintTypeApplication signatureName signatureArguments, TDataType typeName typeArguments)
      | normalizeConstraintSignatureName (identifierText signatureName)
          == normalizeConstraintSignatureName (identifierText typeName),
        length signatureArguments == length typeArguments ->
          and (zipWith (constraintSignatureTypeMatchesExpressionType state) signatureArguments typeArguments)
    (ConstraintTypeFunction signatureArgument signatureResult, TFunctionType argumentType resultType) ->
      constraintSignatureTypeMatchesExpressionType state signatureArgument argumentType
        && constraintSignatureTypeMatchesExpressionType state signatureResult resultType
    _ ->
      case expressionTypeToRuntimeHint (defaultLiteralTypes (resolveType state expressionType)) of
        Just argumentHint -> constraintSignatureTypesCompatible signatureType argumentHint
        Nothing -> False

numericTypeFromConstraintSignatureName :: Text -> Maybe NumericType
numericTypeFromConstraintSignatureName =
  numericTypeFromName . normalizeConstraintSignatureName

concreteImplFactExists :: Text -> ConstraintSignatureType -> ScopeCapabilityFacts -> Bool
concreteImplFactExists constraintName argumentHint facts =
  any
    (\candidateHint -> Set.member (constraintName <> "(" <> renderConstraintSignatureType candidateHint <> ")") (scopeConcreteImplFacts facts))
    (constraintSignatureAliasVariants argumentHint)

concreteImplMethodBodyExists :: Text -> ConstraintSignatureType -> ScopeCapabilityFacts -> Bool
concreteImplMethodBodyExists methodKey argumentHint facts =
  any
    (\(ImplMethodType implTarget) -> constraintSignatureTypesCompatible implTarget argumentHint)
    (Map.findWithDefault [] methodKey (scopeConcreteImplMethods facts))

inferredEqualityConstraintCanUseStructuralRuntimeEquality :: InferState -> ScopeCapabilityFacts -> Maybe Text -> Text -> ExpressionType -> Bool
inferredEqualityConstraintCanUseStructuralRuntimeEquality state facts maybeMethodKey constraintName argumentType =
  maybeMethodKey == Nothing
    && equalityConstraintNameCanUseStructuralRuntimeEquality state facts constraintName
    && structuralRuntimeEqualityType state argumentType

equalityConstraintNameCanUseStructuralRuntimeEquality :: InferState -> ScopeCapabilityFacts -> Text -> Bool
equalityConstraintNameCanUseStructuralRuntimeEquality state facts constraintName =
  activeEqualityClassName state == Just constraintName
    || generatedHiddenEqualityClassFact constraintName facts

generatedHiddenEqualityClassFact :: Text -> ScopeCapabilityFacts -> Bool
generatedHiddenEqualityClassFact constraintName facts =
  Text.isPrefixOf "__module::" constraintName
    && Text.isSuffixOf "::Eq" constraintName
    && Map.lookup constraintName (scopeClassFacts facts) == Just 1

structuralRuntimeEqualityType :: InferState -> ExpressionType -> Bool
structuralRuntimeEqualityType state argumentType =
  case resolveType state argumentType of
    TListType elementType ->
      supportsRuntimeEqualityType state elementType
    TTupleType elementTypes ->
      all (supportsRuntimeEqualityType state) elementTypes
    TDataType typeName typeArguments ->
      dataTypeSupportsRuntimeEquality state typeName typeArguments
    _ ->
      False

instantiateQualifiedMethodType :: Text -> InferState -> Maybe (Maybe ExpressionType, InferState)
instantiateQualifiedMethodType nameText state =
  case splitQualifiedMethodKey nameText of
    Just (capabilityName, _)
      | Map.member capabilityName (inferClassFacts state) ->
          Just (resolveQualifiedMethodType nameText state)
    _ -> Nothing

resolveQualifiedMethodType :: Text -> InferState -> (Maybe ExpressionType, InferState)
resolveQualifiedMethodType methodKey state =
  case Map.lookup methodKey (inferClassMethodSignatures state) of
    Nothing
      | not (null (Map.findWithDefault [] methodKey (inferConcreteImplMethods state))) ->
          (Nothing, state)
      | otherwise ->
          (Nothing, addTypeError state (mkMissingClassMethodError methodKey))
    Just classMethodType ->
      case Map.findWithDefault [] methodKey (inferConcreteImplMethods state) of
        [] ->
          (Nothing, addTypeError state (mkMissingImplMethodBodyError methodKey))
        [implMethodType] ->
          qualifiedMethodSignatureType methodKey classMethodType implMethodType state
        _ ->
          (Nothing, addTypeError state (mkAmbiguousQualifiedMethodBodyError methodKey))

resolveQualifiedMethodApplicationType ::
  Text ->
  TypeEnv ->
  InferState ->
  [(Expr, ExpressionType)] ->
  (Maybe ExpressionType, InferState)
resolveQualifiedMethodApplicationType methodKey env state typedArguments =
  case Map.lookup methodKey (inferClassMethodSignatures state) of
    Nothing
      | not (null (Map.findWithDefault [] methodKey (inferConcreteImplMethods state))) ->
          (Nothing, state)
      | otherwise ->
          (Nothing, addTypeError state (mkMissingClassMethodError methodKey))
    Just classMethodType ->
      case inferQualifiedMethodRequirement methodKey classMethodType state argumentTypes of
        Just inferredRequirement ->
          inferredRequirement
        Nothing ->
          case Map.findWithDefault [] methodKey (inferConcreteImplMethods state) of
            [] ->
              (Nothing, addTypeError state (mkMissingImplMethodBodyError methodKey))
            [implMethodType] ->
              applyQualifiedMethodCandidateWithErrors methodKey classMethodType implMethodType state argumentTypes
            implMethodTypes ->
              selectQualifiedMethodCandidate methodKey classMethodType implMethodTypes env state typedArguments
  where
    argumentTypes = map snd typedArguments

inferQualifiedMethodRequirement ::
  Text ->
  ClassMethodType ->
  InferState ->
  [ExpressionType] ->
  Maybe (Maybe ExpressionType, InferState)
inferQualifiedMethodRequirement methodKey (ClassMethodType classParameter methodSignature) state argumentTypes = do
  (capabilityName, _) <- splitQualifiedMethodKey methodKey
  classArity <- Map.lookup capabilityName (inferClassFacts state)
  if classArity /= 1
    then Nothing
    else
      if not (classMethodSignatureHasTargetArgument classParameter methodSignature)
        then Nothing
        else
          let (classTarget, stateAfterClassTarget) = freshTypeVar state
           in do
                methodType <-
                  classMethodPayloadToGenericExpressionType
                    stateAfterClassTarget
                    classParameter
                    classTarget
                    methodSignature
                let (maybeResultType, stateAfterArguments) =
                      applyKnownFunctionArguments methodType argumentTypes stateAfterClassTarget
                    resolvedClassTarget = resolveType stateAfterArguments classTarget
                case maybeResultType of
                  Just resultType
                    | not (Set.null (freeTypeVariables resolvedClassTarget)) ->
                        Just
                          ( Just resultType,
                            addInferredMethodClassConstraint capabilityName methodKey resolvedClassTarget stateAfterArguments
                          )
                  _ ->
                    Nothing

classMethodSignatureHasTargetArgument :: Text -> SignaturePayload -> Bool
classMethodSignatureHasTargetArgument classParameter methodSignature =
  case signaturePayloadConstraintType methodSignature of
    Just signatureType ->
      let (argumentTypes, _) = constraintFunctionArgumentTypes signatureType
       in any (constraintSignatureTypeContainsClassParameter classParameter) argumentTypes
    Nothing ->
      False

selectQualifiedMethodCandidate ::
  Text ->
  ClassMethodType ->
  [ImplMethodType] ->
  TypeEnv ->
  InferState ->
  [(Expr, ExpressionType)] ->
  (Maybe ExpressionType, InferState)
selectQualifiedMethodCandidate methodKey classMethodType implMethodTypes env state typedArguments =
  case preferredCandidates of
    [] ->
      ( Nothing,
        addTypeError state (mkNoMatchingQualifiedMethodBodyError methodKey (resolvedArgumentTypes state))
      )
    [(matchedType, matchedState)] ->
      (Just matchedType, matchedState)
    _ ->
      ( Nothing,
        addTypeError state (mkAmbiguousQualifiedMethodBodyForArgumentsError methodKey (resolvedArgumentTypes state))
      )
  where
    preferredCandidates =
      case exactMatchingCandidates of
        [] -> matchingCandidates
        exactMatches -> exactMatches

    exactMatchingCandidates =
      filterExactMatches matchingCandidatesWithTargets

    matchingCandidates =
      map
        (\(_, matchedType, matchedState) -> (matchedType, matchedState))
        matchingCandidatesWithTargets

    matchingCandidatesWithTargets =
      foldr collectMatch [] implMethodTypes

    collectMatch implMethodType matches =
      case applyQualifiedMethodCandidate methodKey classMethodType implMethodType state argumentTypes of
        (Just matchedType, matchedState) -> (implMethodType, matchedType, matchedState) : matches
        (Nothing, _) -> matches

    filterExactMatches candidates =
      [ (matchedType, matchedState)
        | (implMethodType, matchedType, matchedState) <- candidates,
          qualifiedMethodCandidateExactlyMatchesArguments state env classMethodType implMethodType typedArguments
      ]

    resolvedArgumentTypes stateForRendering =
      map (resolveType stateForRendering) argumentTypes

    argumentTypes = map snd typedArguments

qualifiedMethodCandidateExactlyMatchesArguments ::
  InferState ->
  TypeEnv ->
  ClassMethodType ->
  ImplMethodType ->
  [(Expr, ExpressionType)] ->
  Bool
qualifiedMethodCandidateExactlyMatchesArguments state env (ClassMethodType classParameter methodSignature) (ImplMethodType implTarget) typedArguments =
  case (signaturePayloadConstraintType methodSignature, substituteClassMethodSignature classParameter implTarget methodSignature) of
    (Just genericSignature, Just substitutedSignature) ->
      let (genericArgumentTypes, _) = constraintFunctionArgumentTypes genericSignature
          (candidateArgumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
          suppliedArgumentCount = length typedArguments
          suppliedGenericArgumentTypes = take suppliedArgumentCount genericArgumentTypes
          suppliedCandidateArgumentTypes = take suppliedArgumentCount candidateArgumentTypes
          targetArgumentPositions =
            map (constraintSignatureTypeContainsClassParameter classParameter) suppliedGenericArgumentTypes
       in suppliedArgumentCount <= length genericArgumentTypes
            && suppliedArgumentCount <= length candidateArgumentTypes
            && or targetArgumentPositions
            && and
              ( zipWith3
                  exactCandidateArgumentMatches
                  targetArgumentPositions
                  suppliedCandidateArgumentTypes
                  typedArguments
              )
    _ ->
      False
  where
    exactCandidateArgumentMatches targetArgumentPosition signatureType (argumentExpr, expressionType) =
      not targetArgumentPosition
        || constraintSignatureTypeExactlyMatchesExpressionType state signatureType expressionType
          && constraintSignatureExpressionHasExactEvidence env signatureType argumentExpr

constraintSignatureExpressionHasExactEvidence :: TypeEnv -> ConstraintSignatureType -> Expr -> Bool
constraintSignatureExpressionHasExactEvidence env signatureType argumentExpr =
  case (signatureType, argumentExpr) of
    (ConstraintTypeList elementType, EList elements) ->
      not (null elements)
        && all (constraintSignatureExpressionHasExactEvidence env elementType) elements
    (ConstraintTypeTuple elementTypes, ETuple elements)
      | length elementTypes == length elements ->
          and (zipWith (constraintSignatureExpressionHasExactEvidence env) elementTypes elements)
    (ConstraintTypeApplication typeName typeArguments, EApply {}) ->
      constructorApplicationExpressionHasExactEvidence env typeName typeArguments argumentExpr
        || constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (ConstraintTypeFunction {}, _) ->
      constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (_, EVar {})
      | constraintSignatureTypeContainsList signatureType ->
          constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (_, EApply {})
      | constraintSignatureTypeContainsList signatureType ->
          constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (_, EIf {}) ->
      constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (_, ECase {}) ->
      constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (_, EPatternCase {}) ->
      constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    (_, EBlock {})
      | constraintSignatureTypeContainsList signatureType ->
          constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr
    _ -> True

constraintSignatureExpressionRuntimeHintMatches :: TypeEnv -> ConstraintSignatureType -> Expr -> Bool
constraintSignatureExpressionRuntimeHintMatches env signatureType argumentExpr =
  case constraintSignatureExpressionRuntimeHint env argumentExpr of
    Just runtimeHint -> runtimeHint == signatureType
    Nothing -> False

constraintSignatureExpressionRuntimeHint :: TypeEnv -> Expr -> Maybe ConstraintSignatureType
constraintSignatureExpressionRuntimeHint env argumentExpr =
  constraintSignatureExpressionRuntimeHintWithLocalHints env Map.empty argumentExpr

constraintSignatureExpressionRuntimeHintWithLocalHints ::
  TypeEnv ->
  Map Text ConstraintSignatureType ->
  Expr ->
  Maybe ConstraintSignatureType
constraintSignatureExpressionRuntimeHintWithLocalHints env localHints argumentExpr =
  case argumentExpr of
    EVar referencedName ->
      Map.lookup (identifierText referencedName) localHints
        <|> (Map.lookup (identifierText referencedName) env >>= typeBindingRuntimeHint)
    EApply (EApply dollarExpr functionExpr) _
      | builtinDollarOperatorExpr env dollarExpr ->
          case constraintSignatureExpressionRuntimeHintWithLocalHints env localHints functionExpr of
            Just (ConstraintTypeFunction _ resultType) -> Just resultType
            _ -> Nothing
    EApply functionExpr _ ->
      case constraintSignatureExpressionRuntimeHintWithLocalHints env localHints functionExpr of
        Just (ConstraintTypeFunction _ resultType) -> Just resultType
        _ -> Nothing
    EIf _ thenExpr elseExpr ->
      commonConstraintSignatureExpressionRuntimeHint env localHints [thenExpr, elseExpr]
    ECase _ thenExpr elseExpr ->
      commonConstraintSignatureExpressionRuntimeHint env localHints [thenExpr, elseExpr]
    EPatternCase _ caseArms ->
      commonConstraintSignatureExpressionRuntimeHint env localHints [bodyExpr | CaseArm _ _ bodyExpr <- caseArms]
    EBlock statements ->
      constraintSignatureBlockRuntimeHint env localHints statements
    _ -> Nothing

commonConstraintSignatureExpressionRuntimeHint ::
  TypeEnv ->
  Map Text ConstraintSignatureType ->
  [Expr] ->
  Maybe ConstraintSignatureType
commonConstraintSignatureExpressionRuntimeHint _ _ [] = Nothing
commonConstraintSignatureExpressionRuntimeHint env localHints (firstExpr : restExprs) = do
  firstHint <- constraintSignatureExpressionRuntimeHintWithLocalHints env localHints firstExpr
  if all
    (\expr -> constraintSignatureExpressionRuntimeHintWithLocalHints env localHints expr == Just firstHint)
    restExprs
    then Just firstHint
    else Nothing

constraintSignatureBlockRuntimeHint ::
  TypeEnv ->
  Map Text ConstraintSignatureType ->
  [Statement] ->
  Maybe ConstraintSignatureType
constraintSignatureBlockRuntimeHint env initialLocalHints statements =
  go initialLocalHints Map.empty statements
  where
    go localHints _ [] =
      Nothing
    go localHints _ [SExpr _ expr] =
      constraintSignatureExpressionRuntimeHintWithLocalHints env localHints expr
    go localHints pendingHints (statement : rest) =
      case statement of
        SSignature name _ signaturePayload ->
          let nameText = identifierText name
              nextPendingHints =
                case signaturePayloadRuntimeHint signaturePayload of
                  Just runtimeHint -> Map.insert nameText runtimeHint pendingHints
                  Nothing -> Map.delete nameText pendingHints
           in go localHints nextPendingHints rest
        SLet name _ valueExpr ->
          let nameText = identifierText name
              bindingHint =
                Map.lookup nameText pendingHints
                  <|> constraintSignatureExpressionRuntimeHintWithLocalHints env localHints valueExpr
              nextLocalHints =
                case bindingHint of
                  Just runtimeHint -> Map.insert nameText runtimeHint localHints
                  Nothing -> localHints
           in go nextLocalHints (Map.delete nameText pendingHints) rest
        _ ->
          go localHints pendingHints rest

signaturePayloadRuntimeHint :: SignaturePayload -> Maybe ConstraintSignatureType
signaturePayloadRuntimeHint signaturePayload =
  case signaturePayload of
    SignatureType signatureType ->
      expressionTypeToRuntimeHint (signatureTypeToExpressionType signatureType)
    ConstrainedSignature _ signatureType
      | Set.null (constraintSignatureTypeVariableNames signatureType) ->
          Just signatureType
    ConstrainedSignature _ signatureType ->
      constraintSignatureTypeToExpressionType signatureType >>= expressionTypeToRuntimeHint
    UnsupportedSignature {} ->
      Nothing

typeBindingRuntimeHint :: TypeBinding -> Maybe ConstraintSignatureType
typeBindingRuntimeHint binding =
  case binding of
    PlainTypeBinding bindingType ->
      expressionTypeToRuntimeHint (defaultLiteralTypes bindingType)
    SchemeTypeBinding (TypeScheme schemeVariables _ _ _ _ schemeType)
      | Set.null schemeVariables ->
          expressionTypeToRuntimeHint (defaultLiteralTypes schemeType)
    OperatorAliasSchemeTypeBinding _ (TypeScheme schemeVariables _ _ _ _ schemeType)
      | Set.null schemeVariables ->
          expressionTypeToRuntimeHint (defaultLiteralTypes schemeType)
    _ -> Nothing

constraintSignatureTypeContainsList :: ConstraintSignatureType -> Bool
constraintSignatureTypeContainsList signatureType =
  case signatureType of
    ConstraintTypeList {} -> True
    ConstraintTypeTuple elementTypes ->
      any constraintSignatureTypeContainsList elementTypes
    ConstraintTypeApplication _ typeArguments ->
      any constraintSignatureTypeContainsList typeArguments
    ConstraintTypeFunction argumentType resultType ->
      constraintSignatureTypeContainsList argumentType
        || constraintSignatureTypeContainsList resultType
    ConstraintTypeName {} -> False

constructorApplicationExpressionHasExactEvidence :: TypeEnv -> Identifier -> [ConstraintSignatureType] -> Expr -> Bool
constructorApplicationExpressionHasExactEvidence env typeName typeArguments argumentExpr =
  case constructorExpressionSpine argumentExpr of
    Just (constructorName, constructorArgumentExprs) ->
      case Map.lookup (identifierText constructorName) env of
        Just (ConstructorTypeBinding constructorTypeName typeParameters constructorArgumentTypes)
          | constructorTypeName == typeName,
            length typeParameters == length typeArguments,
            length constructorArgumentTypes == length constructorArgumentExprs ->
              let typeParameterBindings =
                    Map.fromList (zip (map identifierText typeParameters) typeArguments)
               in and
                    ( zipWith
                        (constructorArgumentExpressionHasExactEvidence env typeParameterBindings)
                        constructorArgumentTypes
                        constructorArgumentExprs
                    )
        _ -> False
    Nothing -> False

constructorExpressionSpine :: Expr -> Maybe (Identifier, [Expr])
constructorExpressionSpine expr =
  go [] expr
  where
    go argumentExprs currentExpr =
      case currentExpr of
        EApply functionExpr argumentExpr ->
          go (argumentExpr : argumentExprs) functionExpr
        EVar constructorName ->
          Just (constructorName, argumentExprs)
        _ ->
          Nothing

constructorArgumentExpressionHasExactEvidence :: TypeEnv -> Map Text ConstraintSignatureType -> ConstructorArgumentType -> Expr -> Bool
constructorArgumentExpressionHasExactEvidence env typeParameterBindings constructorArgument argumentExpr =
  case constructorArgument of
    ConstructorArgumentParameter parameterName ->
      case Map.lookup parameterName typeParameterBindings of
        Just concreteArgumentType ->
          constraintSignatureExpressionHasExactEvidence env concreteArgumentType argumentExpr
        Nothing ->
          True
    ConstructorArgumentMonomorphic {} ->
      True
    ConstructorArgumentFresh ->
      True

constraintSignatureTypeExactlyMatchesExpressionType :: InferState -> ConstraintSignatureType -> ExpressionType -> Bool
constraintSignatureTypeExactlyMatchesExpressionType state signatureType expressionType =
  case constraintSignatureTypeToExpressionTypeWithState state Map.empty signatureType of
    Just signatureExpressionType ->
      resolveType state signatureExpressionType == defaultLiteralTypes (resolveType state expressionType)
    Nothing ->
      False

applyQualifiedMethodCandidate ::
  Text ->
  ClassMethodType ->
  ImplMethodType ->
  InferState ->
  [ExpressionType] ->
  (Maybe ExpressionType, InferState)
applyQualifiedMethodCandidate methodKey classMethodType implMethodType state argumentTypes =
  case qualifiedMethodSignatureType methodKey classMethodType implMethodType state of
    (Nothing, nextState) ->
      (Nothing, nextState)
    (Just methodType, stateAfterMethodType) ->
      applyKnownFunctionArguments methodType argumentTypes stateAfterMethodType

applyQualifiedMethodCandidateWithErrors ::
  Text ->
  ClassMethodType ->
  ImplMethodType ->
  InferState ->
  [ExpressionType] ->
  (Maybe ExpressionType, InferState)
applyQualifiedMethodCandidateWithErrors methodKey classMethodType implMethodType state argumentTypes =
  case qualifiedMethodSignatureType methodKey classMethodType implMethodType state of
    (Nothing, nextState) ->
      (Nothing, nextState)
    (Just methodType, stateAfterMethodType) ->
      applyKnownFunctionArgumentsWithErrors methodType argumentTypes stateAfterMethodType

applyKnownFunctionArguments ::
  ExpressionType ->
  [ExpressionType] ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyKnownFunctionArguments functionType argumentTypes state =
  foldl' step (Just functionType, state) argumentTypes
  where
    step (Nothing, stateAcc) _ =
      (Nothing, stateAcc)
    step (Just currentFunctionType, stateAcc) argumentType =
      let (resultTypeVar, stateWithResultVar) = freshTypeVar stateAcc
       in case unifyTypes currentFunctionType (TFunctionType argumentType resultTypeVar) stateWithResultVar of
            Just unifiedState ->
              (Just (resolveType unifiedState resultTypeVar), unifiedState)
            Nothing ->
              (Nothing, stateAcc)

applyKnownFunctionArgumentsWithErrors ::
  ExpressionType ->
  [ExpressionType] ->
  InferState ->
  (Maybe ExpressionType, InferState)
applyKnownFunctionArgumentsWithErrors functionType argumentTypes state =
  foldl' step (Just functionType, state) argumentTypes
  where
    step (Nothing, stateAcc) _ =
      (Nothing, stateAcc)
    step (Just currentFunctionType, stateAcc) argumentType =
      let (resultTypeVar, stateWithResultVar) = freshTypeVar stateAcc
       in case unifyTypes currentFunctionType (TFunctionType argumentType resultTypeVar) stateWithResultVar of
            Just unifiedState ->
              (Just (resolveType unifiedState resultTypeVar), unifiedState)
            Nothing ->
              ( Nothing,
                addTypeError
                  stateWithResultVar
                  ( mkApplyTypeError
                      (resolveType stateWithResultVar currentFunctionType)
                      (resolveType stateWithResultVar argumentType)
                  )
              )

qualifiedMethodSignatureType ::
  Text ->
  ClassMethodType ->
  ImplMethodType ->
  InferState ->
  (Maybe ExpressionType, InferState)
qualifiedMethodSignatureType methodKey (ClassMethodType classParameter methodSignature) (ImplMethodType implTarget) state =
  case classMethodPayloadToExpressionType state classParameter implTarget methodSignature of
    Just methodType -> (Just methodType, state)
    Nothing ->
      (Nothing, addTypeError state (mkInvalidQualifiedMethodSignatureError methodKey methodSignature))

classMethodPayloadToExpressionType ::
  InferState ->
  Text ->
  ConstraintSignatureType ->
  SignaturePayload ->
  Maybe ExpressionType
classMethodPayloadToExpressionType state classParameter implTarget methodSignature =
  substituteClassMethodSignature classParameter implTarget methodSignature
    >>= constraintSignatureTypeToExpressionTypeWithState state Map.empty

classMethodPayloadToGenericExpressionType ::
  InferState ->
  Text ->
  ExpressionType ->
  SignaturePayload ->
  Maybe ExpressionType
classMethodPayloadToGenericExpressionType state classParameter classTarget methodSignature =
  signaturePayloadConstraintType methodSignature
    >>= constraintSignatureTypeToExpressionTypeWithState
      state
      (Map.singleton classParameter classTarget)

constraintSignatureTypeToExpressionTypeWithState ::
  InferState ->
  Map Text ExpressionType ->
  ConstraintSignatureType ->
  Maybe ExpressionType
constraintSignatureTypeToExpressionTypeWithState state signatureVariables signatureType =
  case signatureType of
    ConstraintTypeName name ->
      case identifierText name of
        "Int" -> Just TIntType
        "Float" -> Just TFloatType
        "Bool" -> Just TBoolType
        typeName ->
          case numericTypeNameToExpressionType typeName of
            Just numericType -> Just numericType
            Nothing ->
              case Map.lookup typeName signatureVariables of
                Just variableType -> Just variableType
                Nothing
                  | Map.member typeName (inferDataTypes state) ->
                      Just (TDataType name [])
                  | otherwise ->
                      Nothing
    ConstraintTypeApplication name arguments
      | Map.member (identifierText name) (inferDataTypes state) ->
          TDataType name <$> traverse (constraintSignatureTypeToExpressionTypeWithState state signatureVariables) arguments
      | otherwise ->
          Nothing
    ConstraintTypeList innerType ->
      TListType <$> constraintSignatureTypeToExpressionTypeWithState state signatureVariables innerType
    ConstraintTypeTuple elementTypes ->
      TTupleType <$> traverse (constraintSignatureTypeToExpressionTypeWithState state signatureVariables) elementTypes
    ConstraintTypeFunction argumentType resultType ->
      TFunctionType
        <$> constraintSignatureTypeToExpressionTypeWithState state signatureVariables argumentType
        <*> constraintSignatureTypeToExpressionTypeWithState state signatureVariables resultType

instantiateConstructorBinding :: TypeBinding -> InferState -> Maybe ([ExpressionType], ExpressionType, InferState)
instantiateConstructorBinding binding state =
  case binding of
    ConstructorTypeBinding typeName typeParameters argumentTypes ->
      Just (instantiateConstructorType typeName typeParameters argumentTypes state)
    _ -> Nothing

instantiateConstructorType ::
  Identifier ->
  [Identifier] ->
  [ConstructorArgumentType] ->
  InferState ->
  ([ExpressionType], ExpressionType, InferState)
instantiateConstructorType typeName typeParameters argumentTypes state =
  let (typeParameterBindings, resultParameterTypes, stateAfterParameters) =
        instantiateConstructorTypeParameters typeParameters state
      (constructorArgumentTypesRev, stateAfterArguments) =
        instantiateConstructorArguments typeParameterBindings argumentTypes stateAfterParameters
   in
    ( reverse constructorArgumentTypesRev,
      TDataType typeName (reverse resultParameterTypes),
      stateAfterArguments
    )

instantiateConstructorTypeParameters ::
  [Identifier] ->
  InferState ->
  (Map Text ExpressionType, [ExpressionType], InferState)
instantiateConstructorTypeParameters typeParameters state =
  foldl' step (Map.empty, [], state) typeParameters
  where
    step (bindings, parameterTypesRev, stateAcc) typeParameter =
      let (parameterType, nextState) = freshTypeVar stateAcc
       in
        ( Map.insert (identifierText typeParameter) parameterType bindings,
          parameterType : parameterTypesRev,
          nextState
        )

instantiateConstructorArguments ::
  Map Text ExpressionType ->
  [ConstructorArgumentType] ->
  InferState ->
  ([ExpressionType], InferState)
instantiateConstructorArguments typeParameterBindings argumentTypes initialState =
  foldl' step ([], initialState) argumentTypes
  where
    step (argumentTypesRev, stateAcc) argumentType =
      case argumentType of
        ConstructorArgumentMonomorphic expressionType ->
          (resolveType stateAcc expressionType : argumentTypesRev, stateAcc)
        ConstructorArgumentParameter parameterName ->
          case Map.lookup parameterName typeParameterBindings of
            Just parameterType ->
              (parameterType : argumentTypesRev, stateAcc)
            Nothing ->
              let (freshArgumentType, nextState) = freshTypeVar stateAcc
               in
                ( freshArgumentType : argumentTypesRev,
                  addTypeError nextState (mkMissingConstructorTypeParameterBindingError parameterName)
                )
        ConstructorArgumentFresh ->
          let (freshArgumentType, nextState) = freshTypeVar stateAcc
           in (freshArgumentType : argumentTypesRev, nextState)

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

data SignaturePayloadType = SignaturePayloadType
  { signaturePayloadDeclaredType :: ExpressionType,
    signaturePayloadExplicitConstraints :: [TypeSchemeConstraint],
    signaturePayloadVariableOrder :: [Int]
  }

-- | Normalize the currently accepted signature subset. Unsupported surfaces
-- return `Nothing` so callers can emit the stable signature diagnostic.
signaturePayloadToSignatureType :: SignaturePayload -> InferState -> (Maybe SignaturePayloadType, InferState)
signaturePayloadToSignatureType signaturePayload state =
  case signaturePayload of
    SignatureType signatureType ->
      (Just (SignaturePayloadType (signatureTypeToExpressionType signatureType) [] []), state)
    ConstrainedSignature [] signatureType ->
      (fmap (\declaredType -> SignaturePayloadType declaredType [] []) (constraintSignatureTypeToExpressionType signatureType), state)
    ConstrainedSignature constraints signatureType
      | supportedConcreteConstraints state constraints ->
          (fmap (\declaredType -> SignaturePayloadType declaredType [] []) (constraintSignatureTypeToExpressionType signatureType), state)
      | supportedVariableConstraints state constraints signatureType ->
          variableConstraintSignaturePayloadToExpressionType constraints signatureType state
      | otherwise ->
          (Nothing, state)
    UnsupportedSignature {} ->
      (Nothing, state)

signatureTypeToExpressionType :: SignatureType -> ExpressionType
signatureTypeToExpressionType signatureType =
  case signatureType of
    TypeInt -> TIntType
    TypeFloat -> TFloatType
    TypeNumeric numericType -> TNumericType numericType
    TypeBool -> TBoolType
    TypeList innerType ->
      TListType (signatureTypeToExpressionType innerType)
    TypeTuple elementTypes ->
      TTupleType (map signatureTypeToExpressionType elementTypes)
    TypeFunction argumentType resultType ->
      TFunctionType
        (signatureTypeToExpressionType argumentType)
        (signatureTypeToExpressionType resultType)

constraintSignatureTypeToExpressionType :: ConstraintSignatureType -> Maybe ExpressionType
constraintSignatureTypeToExpressionType =
  constraintSignatureTypeToExpressionTypeWithVariables Map.empty

constraintSignatureTypeToExpressionTypeWithVariables ::
  Map Text ExpressionType ->
  ConstraintSignatureType ->
  Maybe ExpressionType
constraintSignatureTypeToExpressionTypeWithVariables signatureVariables signatureType =
  case signatureType of
    ConstraintTypeName name ->
      case identifierText name of
        "Int" -> Just TIntType
        "Float" -> Just TFloatType
        "Bool" -> Just TBoolType
        typeName ->
          case numericTypeNameToExpressionType typeName of
            Just numericType -> Just numericType
            Nothing -> Map.lookup typeName signatureVariables
    ConstraintTypeApplication {} ->
      Nothing
    ConstraintTypeList innerType ->
      TListType <$> constraintSignatureTypeToExpressionTypeWithVariables signatureVariables innerType
    ConstraintTypeTuple elementTypes ->
      TTupleType <$> traverse (constraintSignatureTypeToExpressionTypeWithVariables signatureVariables) elementTypes
    ConstraintTypeFunction argumentType resultType ->
      TFunctionType
        <$> constraintSignatureTypeToExpressionTypeWithVariables signatureVariables argumentType
        <*> constraintSignatureTypeToExpressionTypeWithVariables signatureVariables resultType

variableConstraintSignaturePayloadToExpressionType ::
  [SignatureConstraint] ->
  ConstraintSignatureType ->
  InferState ->
  (Maybe SignaturePayloadType, InferState)
variableConstraintSignaturePayloadToExpressionType constraints signatureType state =
  let variableNames = constraintSignatureTypeVariableNamesInOrder signatureType
      (signatureVariables, nextState) = allocateSignatureTypeVariables variableNames state
      convertedType =
        constraintSignatureTypeToExpressionTypeWithVariables signatureVariables signatureType
      convertedConstraints =
        traverse (variableConstraintToTypeSchemeConstraint signatureVariables) constraints
      variableOrder =
        [ typeVar
          | variableName <- variableNames,
            Just (TVarType typeVar) <- [Map.lookup variableName signatureVariables]
        ]
   in
    case (convertedType, convertedConstraints) of
      (Just expressionType, Just explicitConstraints) ->
        (Just (SignaturePayloadType expressionType explicitConstraints variableOrder), nextState)
      _ -> (Nothing, state)

variableConstraintToTypeSchemeConstraint ::
  Map Text ExpressionType ->
  SignatureConstraint ->
  Maybe TypeSchemeConstraint
variableConstraintToTypeSchemeConstraint signatureVariables (SignatureConstraint constraintName arguments) =
  case arguments of
    [ConstraintTypeName argumentName] ->
      TypeSchemeConstraint (identifierText constraintName)
        <$> Map.lookup (identifierText argumentName) signatureVariables
    _ -> Nothing

allocateSignatureTypeVariables :: [Text] -> InferState -> (Map Text ExpressionType, InferState)
allocateSignatureTypeVariables variableNames state =
  foldl' allocate (Map.empty, state) variableNames
  where
    allocate (signatureVariables, stateAcc) variableName =
      let (variableType, nextState) = freshTypeVar stateAcc
       in (Map.insert variableName variableType signatureVariables, nextState)

supportedConcreteConstraints :: InferState -> [SignatureConstraint] -> Bool
supportedConcreteConstraints state constraints =
  not (null constraints)
    && isNothing (duplicateConstraintName constraints)
    && all (supportedConcreteConstraint state) constraints

-- | Variable constrained signatures are accepted when every constrained
-- variable appears in the body; extra body variables remain unconstrained.
supportedVariableConstraints :: InferState -> [SignatureConstraint] -> ConstraintSignatureType -> Bool
supportedVariableConstraints state constraints signatureType =
  not (null constraints)
    && isNothing (duplicateConstraintName constraints)
    && all (supportedVariableConstraint state) constraints
    && constraintSignatureTypeSupportsVariableBody signatureType
    && not (Set.null signatureVariableNames)
    && constraintVariableNames `Set.isSubsetOf` signatureVariableNames
  where
    signatureVariableNames =
      constraintSignatureTypeVariableNames signatureType
    constraintVariableNames =
      Set.unions (map constraintVariableNamesInSupportedConstraint constraints)

supportedConcreteConstraint :: InferState -> SignatureConstraint -> Bool
supportedConcreteConstraint state (SignatureConstraint constraintName arguments) =
  case (Map.lookup (identifierText constraintName) (inferClassFacts state), arguments) of
    (Just 1, [argument]) ->
      concreteConstraintArgument argument
        && Set.member
          (constraintImplFactKey constraintName argument)
          (inferConcreteImplFacts state)
    _ -> False

supportedVariableConstraint :: InferState -> SignatureConstraint -> Bool
supportedVariableConstraint state (SignatureConstraint constraintName arguments) =
  case (Map.lookup (identifierText constraintName) (inferClassFacts state), arguments) of
    (Just 1, [ConstraintTypeName argumentName]) ->
      identifierLooksLikeTypeVariable argumentName
    _ -> False

constraintVariableNamesInSupportedConstraint :: SignatureConstraint -> Set Text
constraintVariableNamesInSupportedConstraint constraint =
  case constraint of
    SignatureConstraint _ [ConstraintTypeName argumentName]
      | identifierLooksLikeTypeVariable argumentName ->
          Set.singleton (identifierText argumentName)
    _ -> Set.empty

constraintSignatureTypeVariableNames :: ConstraintSignatureType -> Set Text
constraintSignatureTypeVariableNames signatureType =
  case signatureType of
    ConstraintTypeName name
      | identifierLooksLikeTypeVariable name ->
          Set.singleton (identifierText name)
      | otherwise ->
          Set.empty
    ConstraintTypeApplication _ arguments ->
      Set.unions (map constraintSignatureTypeVariableNames arguments)
    ConstraintTypeList innerType ->
      constraintSignatureTypeVariableNames innerType
    ConstraintTypeTuple elementTypes ->
      Set.unions (map constraintSignatureTypeVariableNames elementTypes)
    ConstraintTypeFunction argumentType resultType ->
      Set.union
        (constraintSignatureTypeVariableNames argumentType)
        (constraintSignatureTypeVariableNames resultType)

constraintSignatureTypeVariableNamesInOrder :: ConstraintSignatureType -> [Text]
constraintSignatureTypeVariableNamesInOrder =
  dedupe . go
  where
    go signatureType =
      case signatureType of
        ConstraintTypeName name
          | identifierLooksLikeTypeVariable name ->
              [identifierText name]
          | otherwise ->
              []
        ConstraintTypeApplication _ arguments ->
          concatMap go arguments
        ConstraintTypeList innerType ->
          go innerType
        ConstraintTypeTuple elementTypes ->
          concatMap go elementTypes
        ConstraintTypeFunction argumentType resultType ->
          go argumentType ++ go resultType

    dedupe =
      goDedupe Set.empty

    goDedupe _ [] = []
    goDedupe seen (name : rest)
      | Set.member name seen = goDedupe seen rest
      | otherwise = name : goDedupe (Set.insert name seen) rest

constraintSignatureTypeSupportsVariableBody :: ConstraintSignatureType -> Bool
constraintSignatureTypeSupportsVariableBody signatureType =
  case signatureType of
    ConstraintTypeName {} -> True
    ConstraintTypeApplication {} -> False
    ConstraintTypeList innerType ->
      constraintSignatureTypeSupportsVariableBody innerType
    ConstraintTypeTuple elementTypes ->
      all constraintSignatureTypeSupportsVariableBody elementTypes
    ConstraintTypeFunction argumentType resultType ->
      constraintSignatureTypeSupportsVariableBody argumentType
        && constraintSignatureTypeSupportsVariableBody resultType

numericTypeNameToExpressionType :: Text -> Maybe ExpressionType
numericTypeNameToExpressionType typeName =
  TNumericType <$> numericTypeFromName typeName

integerLiteralRangeFitsNumericType :: IntegerLiteralRange -> NumericType -> Bool
integerLiteralRangeFitsNumericType literalRange numericType =
  case numericTypeIntegerBounds numericType of
    Just (lowerBound, upperBound) ->
      let (literalMin, literalMax) = integerLiteralRangeBounds literalRange
       in literalMin >= lowerBound && literalMax <= upperBound
    Nothing -> False

numericConversionLiteralDiagnostic :: BuiltinResolutionMode -> TypeEnv -> Expr -> Expr -> Maybe Diagnostic
numericConversionLiteralDiagnostic builtinMode env functionExpr argumentExpr =
  case (functionExpr, argumentExpr) of
    (EVar functionName, ELit (LInt literalValue)) ->
      case numericConversionTargetFromCallable builtinMode env functionName of
        Just targetType ->
          case numericTypeLiteralIntegerBounds targetType of
            Just bounds@(lowerBound, upperBound)
              | literalValue < lowerBound || literalValue > upperBound ->
                  Just (mkNumericConversionLiteralTypeError (identifierText functionName) literalValue targetType bounds)
            _ -> Nothing
        Nothing -> Nothing
    (EVar functionName, ELit (LFloat literalValue literalSource _)) ->
      case numericConversionTargetFromCallable builtinMode env functionName of
        Just targetType ->
          numericConversionFloatLiteralDiagnostic
            (identifierText functionName)
            targetType
            literalValue
            literalSource
        Nothing -> Nothing
    _ -> Nothing

numericConversionFloatLiteralDiagnostic :: Text -> NumericType -> Double -> FractionalLiteralSource -> Maybe Diagnostic
numericConversionFloatLiteralDiagnostic conversionName targetType literalValue literalSource =
  case numericTypeIntegerBounds targetType of
    Just bounds@(lowerBound, upperBound) ->
      case fractionalLiteralIntegralValue literalSource of
        Just integralValue
          | finiteFloat literalValue,
            integralValue >= lowerBound,
            integralValue <= upperBound ->
              Nothing
        _ ->
          Just (mkNumericConversionFractionalLiteralTypeError conversionName literalValue targetType bounds)
    Nothing ->
      case numericTypeFloatMax targetType of
        Just maxMagnitude
          | not (finiteFloat literalValue)
              || abs literalValue > maxMagnitude
              || fractionalLiteralExceedsMagnitude literalSource maxMagnitude ->
              Just (mkNumericConversionFloatLiteralOverflowError conversionName literalValue targetType maxMagnitude)
        _ -> Nothing

targetedFloatLiteralDiagnostic :: NumericType -> Double -> FractionalLiteralSource -> Maybe Diagnostic
targetedFloatLiteralDiagnostic targetType literalValue literalSource =
  case numericTypeFloatMax targetType of
    Just maxMagnitude
      | not (finiteFloat literalValue)
          || abs literalValue > maxMagnitude
          || fractionalLiteralExceedsMagnitude literalSource maxMagnitude ->
          Just (mkTargetedFractionalLiteralOverflowError literalValue targetType maxMagnitude)
    _ -> Nothing

finiteFloat :: Double -> Bool
finiteFloat value = not (isNaN value) && not (isInfinite value)

numericConversionTargetFromCallable :: BuiltinResolutionMode -> TypeEnv -> Identifier -> Maybe NumericType
numericConversionTargetFromCallable builtinMode env functionName =
  let nameText = identifierText functionName
   in case Map.lookup nameText env of
        Just (BuiltinAliasTypeBinding builtinSymbol) ->
          builtinSymbolNumericConversionTarget builtinSymbol
        Just _ ->
          Nothing
        Nothing ->
          lookupBuiltinSymbolInMode builtinMode nameText >>= builtinSymbolNumericConversionTarget

singletonIntegerLiteralRange :: Integer -> IntegerLiteralRange
singletonIntegerLiteralRange value = IntegerLiteralRange value value

numericLiteralBinaryRange ::
  Text ->
  NumericRuleResult ->
  IntegerLiteralRange ->
  IntegerLiteralRange ->
  IntegerLiteralRange
numericLiteralBinaryRange operatorSymbol resultRule leftRange rightRange =
  case resultRule of
    NumericSameTypeResult ->
      let operandRange = combineIntegerLiteralRanges leftRange rightRange
       in case integerLiteralArithmeticResultRange operatorSymbol leftRange rightRange of
            Just resultRange -> combineIntegerLiteralRanges operandRange resultRange
            Nothing -> operandRange
    NumericBoolResult ->
      combineIntegerLiteralRanges leftRange rightRange

integerLiteralArithmeticResultRange ::
  Text ->
  IntegerLiteralRange ->
  IntegerLiteralRange ->
  Maybe IntegerLiteralRange
integerLiteralArithmeticResultRange operatorSymbol (IntegerLiteralRange leftMin leftMax) (IntegerLiteralRange rightMin rightMax) =
  case operatorSymbol of
    "+" -> Just (IntegerLiteralRange (leftMin + rightMin) (leftMax + rightMax))
    "-" -> Just (IntegerLiteralRange (leftMin - rightMax) (leftMax - rightMin))
    "*" -> Just (rangeFromValues [leftMin * rightMin, leftMin * rightMax, leftMax * rightMin, leftMax * rightMax])
    "/"
      | rightMin <= 0 && rightMax >= 0 -> Nothing
      | otherwise ->
          Just
            ( rangeFromValues
                [ leftMin `div` rightMin,
                  leftMin `div` rightMax,
                  leftMax `div` rightMin,
                  leftMax `div` rightMax
                ]
            )
    _ -> Nothing

rangeFromValues :: [Integer] -> IntegerLiteralRange
rangeFromValues values = IntegerLiteralRange (minimum values) (maximum values)

mergedUnifiedType :: InferState -> ExpressionType -> ExpressionType -> ExpressionType
mergedUnifiedType state leftType rightType =
  mergeIntegerLiteralRanges (resolveType state leftType) (resolveType state rightType)

mergeIntegerLiteralRanges :: ExpressionType -> ExpressionType -> ExpressionType
mergeIntegerLiteralRanges leftType rightType =
  case (leftType, rightType) of
    (TIntegerLiteralType leftRange, TIntegerLiteralType rightRange) ->
      TIntegerLiteralType (combineIntegerLiteralRanges leftRange rightRange)
    (TIntegerLiteralType literalRange, numericType@(TNumericType concreteNumericType))
      | integerLiteralRangeFitsNumericType literalRange concreteNumericType -> numericType
    (numericType@(TNumericType concreteNumericType), TIntegerLiteralType literalRange)
      | integerLiteralRangeFitsNumericType literalRange concreteNumericType -> numericType
    (TIntegerLiteralType {}, TIntType) -> TIntType
    (TIntType, TIntegerLiteralType {}) -> TIntType
    (TListType leftElementType, TListType rightElementType) ->
      TListType (mergeIntegerLiteralRanges leftElementType rightElementType)
    (TTupleType leftElementTypes, TTupleType rightElementTypes)
      | length leftElementTypes == length rightElementTypes ->
          TTupleType (zipWith mergeIntegerLiteralRanges leftElementTypes rightElementTypes)
    (TDataType leftName leftArguments, TDataType rightName rightArguments)
      | leftName == rightName,
        length leftArguments == length rightArguments ->
          TDataType leftName (zipWith mergeIntegerLiteralRanges leftArguments rightArguments)
    (TFunctionType leftInputType leftOutputType, TFunctionType rightInputType rightOutputType) ->
      TFunctionType
        (mergeIntegerLiteralRanges leftInputType rightInputType)
        (mergeIntegerLiteralRanges leftOutputType rightOutputType)
    _ -> leftType

combineIntegerLiteralRanges :: IntegerLiteralRange -> IntegerLiteralRange -> IntegerLiteralRange
combineIntegerLiteralRanges (IntegerLiteralRange leftMin leftMax) (IntegerLiteralRange rightMin rightMax) =
  IntegerLiteralRange (min leftMin rightMin) (max leftMax rightMax)

integerLiteralRangeBounds :: IntegerLiteralRange -> (Integer, Integer)
integerLiteralRangeBounds (IntegerLiteralRange lower upper) =
  (lower, upper)

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

renderSignatureType :: SignatureType -> Text
renderSignatureType signatureType =
  case signatureType of
    TypeInt -> "Int"
    TypeFloat -> "Float"
    TypeNumeric numericType -> renderNumericTypeName numericType
    TypeBool -> "Bool"
    TypeList innerType ->
      "[" <> renderListElementSignatureType innerType <> "]"
    TypeTuple elementTypes ->
      "(" <> Text.intercalate ", " (map renderSignatureType elementTypes) <> ")"
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
    Just (NumericRule resultRule) ->
      let (typeVar, operandType, stateAfterOperandType) = freshTypeVariable state
          stateAfterNumericConstraint =
            addNumericTypeVarConstraint typeVar (numericRuleConstraint resultRule) stateAfterOperandType
       in
        Just
          ( TFunctionType
              operandType
              (TFunctionType operandType (numericRuleResultType resultRule operandType)),
            stateAfterNumericConstraint
          )
    Just StrictEqualityRule ->
      let (typeVar, operandType, stateAfterOperandType) = freshTypeVariable state
       in
        Just
          ( TFunctionType operandType (TFunctionType operandType TBoolType),
            addInferredEqualityClassConstraintIfVisible
              operandType
              (addStrictEqualityTypeVarConstraint typeVar stateAfterOperandType)
          )
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

instantiateDeclaredOperatorBindingType :: TypeEnv -> Text -> InferState -> (Maybe ExpressionType, InferState)
instantiateDeclaredOperatorBindingType env operatorSymbol state =
  case Map.lookup (operatorBindingIdentifierText operatorSymbol) env of
    Just binding ->
      instantiateTypeBinding binding state
    Nothing ->
      ( Nothing,
        addTypeError state (mkMissingOperatorBindingError operatorSymbol)
      )

declaredOperatorRightSectionExpr :: Text -> Expr -> Expr
declaredOperatorRightSectionExpr operatorSymbol rightExpr =
  ELambda
    leftParameter
    (EApply (EApply (EOperatorValue operatorSymbol) (EVar leftParameter)) rightExpr)
  where
    leftParameter = mkIdentifier "$operator_section_left"

-- | Instantiate builtin symbol types on demand so each use site gets fresh type
-- variables instead of sharing one global schematic type.
instantiateBuiltinSymbolType :: BuiltinSymbol -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinSymbolType builtinSymbol state =
  -- Use catalog names here so newly-added symbols safely fall back to `Nothing`
  -- until an explicit type-instantiation rule is defined.
  case builtinSymbolNumericConversionTarget builtinSymbol of
    Just targetType ->
      let (sourceTypeVar, sourceType, stateAfterSourceType) = freshTypeVariable state
          stateAfterNumericConstraint =
            addNumericTypeVarConstraint sourceTypeVar AnyNumericConstraint stateAfterSourceType
       in Just (TFunctionType sourceType (TNumericType targetType), stateAfterNumericConstraint)
    Nothing ->
      instantiateBuiltinSymbolTypeByName (builtinSymbolName builtinSymbol) state

instantiateBuiltinSymbolTypeByName :: Text -> InferState -> Maybe (ExpressionType, InferState)
instantiateBuiltinSymbolTypeByName builtinName state =
  case builtinName of
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
  let (_, expressionType, nextState) = freshTypeVariable state
   in (expressionType, nextState)

freshTypeVariable :: InferState -> (Int, ExpressionType, InferState)
freshTypeVariable state =
  let nextVar = inferNextTypeVar state
   in (nextVar, TVarType nextVar, state {inferNextTypeVar = nextVar + 1})

resolveType :: InferState -> ExpressionType -> ExpressionType
resolveType state = applySubstitution (inferSubst state)

defaultLiteralTypes :: ExpressionType -> ExpressionType
defaultLiteralTypes =
  defaultLiteralTypesWith TIntType

defaultBindingLiteralTypes :: ExpressionType -> ExpressionType
defaultBindingLiteralTypes =
  defaultLiteralTypesWith (TNumericType NumericInt64)

defaultLiteralTypesWith :: ExpressionType -> ExpressionType -> ExpressionType
defaultLiteralTypesWith integerLiteralDefault expressionType =
  case expressionType of
    TIntegerLiteralType {} -> integerLiteralDefault
    TListType elementType ->
      TListType (defaultLiteralTypesWith integerLiteralDefault elementType)
    TTupleType elementTypes ->
      TTupleType (map (defaultLiteralTypesWith integerLiteralDefault) elementTypes)
    TDataType typeName typeArguments ->
      TDataType typeName (map (defaultLiteralTypesWith integerLiteralDefault) typeArguments)
    TFunctionType inputType outputType ->
      TFunctionType
        (defaultLiteralTypesWith integerLiteralDefault inputType)
        (defaultLiteralTypesWith integerLiteralDefault outputType)
    _ -> expressionType

runtimeHintFromExpressionType :: InferState -> ExpressionType -> Maybe ConstraintSignatureType
runtimeHintFromExpressionType state expressionType =
  expressionTypeToRuntimeHint (defaultLiteralTypes (resolveType state expressionType))

expressionTypeToRuntimeHint :: ExpressionType -> Maybe ConstraintSignatureType
expressionTypeToRuntimeHint expressionType =
  case expressionType of
    TIntType -> Just (ConstraintTypeName "Int")
    TIntegerLiteralType literalRange
      | integerLiteralRangeFitsNumericType literalRange NumericInt64 ->
          Just (ConstraintTypeName "Int")
      | otherwise -> Nothing
    TFloatType -> Just (ConstraintTypeName "Float")
    TNumericType numericType -> Just (ConstraintTypeName (mkIdentifier (renderNumericTypeName numericType)))
    TBoolType -> Just (ConstraintTypeName "Bool")
    TListType elementType ->
      ConstraintTypeList <$> expressionTypeToRuntimeHint elementType
    TTupleType elementTypes ->
      ConstraintTypeTuple <$> traverse expressionTypeToRuntimeHint elementTypes
    TDataType typeName typeArguments ->
      case traverse expressionTypeToRuntimeHint typeArguments of
        Just [] -> Just (ConstraintTypeName typeName)
        Just argumentHints ->
          Just (ConstraintTypeApplication typeName argumentHints)
        Nothing -> Nothing
    TFunctionType inputType outputType ->
      ConstraintTypeFunction
        <$> expressionTypeToRuntimeHint inputType
        <*> expressionTypeToRuntimeHint outputType
    TVarType {} -> Nothing

applySubstitution :: Map Int ExpressionType -> ExpressionType -> ExpressionType
applySubstitution subst expressionType =
  case expressionType of
    TIntType -> TIntType
    TIntegerLiteralType literalRange -> TIntegerLiteralType literalRange
    TFloatType -> TFloatType
    TNumericType numericType -> TNumericType numericType
    TBoolType -> TBoolType
    TListType elementType -> TListType (applySubstitution subst elementType)
    TTupleType elementTypes -> TTupleType (map (applySubstitution subst) elementTypes)
    TDataType typeName typeArguments ->
      TDataType typeName (map (applySubstitution subst) typeArguments)
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
        (TVarType leftVar, _) -> bindTypeVar leftVar resolvedRight state
        (_, TVarType rightVar) -> bindTypeVar rightVar resolvedLeft state
        _ -> Nothing

unifyTypeLists :: [ExpressionType] -> [ExpressionType] -> InferState -> Maybe InferState
unifyTypeLists leftTypes rightTypes state =
  if length leftTypes /= length rightTypes
    then Nothing
    else
      foldl'
        step
        (Just state)
        (zip leftTypes rightTypes)
  where
    step maybeState (leftType, rightType) =
      case maybeState of
        Just stateAcc -> unifyTypes leftType rightType stateAcc
        Nothing -> Nothing

-- | Bind a type variable while preserving the deferred equality constraints
-- introduced by strict-equality operator sections.
bindTypeVar :: Int -> ExpressionType -> InferState -> Maybe InferState
bindTypeVar typeVar replacementType state
  | replacementType == TVarType typeVar = Just state
  | occursInType typeVar replacementType = Nothing
  -- Preserve compile/runtime contract when deferred section vars later unify.
  | typeVarIsStrictEqualityConstrained && not (supportsDeferredEqualityOperandType state replacementType) =
      Nothing
  | otherwise =
      case constrainedReplacementType of
        Nothing -> Nothing
        Just nextReplacementType ->
          Just
            (stateAfterNumericConstraint nextReplacementType)
              { inferSubst = Map.insert typeVar nextReplacementType (inferSubst state),
                inferStrictEqualityVars = nextStrictEqualityVars nextReplacementType
              }
  where
    typeVarIsStrictEqualityConstrained =
      Set.member typeVar (inferStrictEqualityVars state)
    typeVarNumericConstraint =
      Map.lookup typeVar (inferNumericVars state)
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
    numericVarsWithoutTypeVar =
      Map.delete typeVar (inferNumericVars state)
    stateAfterNumericConstraint nextReplacementType =
      case (typeVarNumericConstraint, nextReplacementType) of
        (Just numericConstraint, TVarType replacementVar) ->
          addNumericTypeVarConstraint
            replacementVar
            numericConstraint
            state {inferNumericVars = numericVarsWithoutTypeVar}
        _ ->
          state {inferNumericVars = numericVarsWithoutTypeVar}

occursInType :: Int -> ExpressionType -> Bool
occursInType typeVar expressionType =
  case expressionType of
    TIntType -> False
    TIntegerLiteralType {} -> False
    TFloatType -> False
    TNumericType {} -> False
    TBoolType -> False
    TListType elementType -> occursInType typeVar elementType
    TTupleType elementTypes -> any (occursInType typeVar) elementTypes
    TDataType _ typeArguments -> any (occursInType typeVar) typeArguments
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

addInferredClassConstraint :: Text -> ExpressionType -> InferState -> InferState
addInferredClassConstraint constraintName argumentType state =
  state
    { inferInferredClassConstraints =
        TypeSchemeInferredConstraint constraintName argumentType : inferInferredClassConstraints state
    }

addInferredMethodClassConstraint :: Text -> Text -> ExpressionType -> InferState -> InferState
addInferredMethodClassConstraint constraintName methodKey argumentType state =
  state
    { inferInferredClassConstraints =
        TypeSchemeMethodConstraint constraintName methodKey argumentType : inferInferredClassConstraints state
    }

addInferredEqualityClassConstraintIfVisible :: ExpressionType -> InferState -> InferState
addInferredEqualityClassConstraintIfVisible argumentType state =
  case activeEqualityClassName state of
    Just equalityClassName -> addInferredClassConstraint equalityClassName argumentType state
    Nothing -> state

activeEqualityClassName :: InferState -> Maybe Text
activeEqualityClassName state =
  case inferCurrentModulePath state of
    Just modulePath
      | let replayQualifiedEqName = moduleReplayQualifiedName modulePath "Eq",
        classFactIsUnary replayQualifiedEqName ->
          Just replayQualifiedEqName
    _ ->
      if classFactIsUnary "Eq"
        then Just "Eq"
        else Nothing
  where
    classFactIsUnary className =
      Map.lookup className (inferClassFacts state) == Just 1

moduleReplayQualifiedName :: [Text] -> Text -> Text
moduleReplayQualifiedName modulePath name =
  qualifiedIdentifierText "__module" (Text.intercalate "::" modulePath <> "::" <> name)

addNumericTypeVarConstraint :: Int -> NumericConstraint -> InferState -> InferState
addNumericTypeVarConstraint typeVar numericConstraint state =
  state
    { inferNumericVars =
        Map.insertWith
          combineNumericConstraints
          typeVar
          numericConstraint
          (inferNumericVars state)
    }

combineNumericConstraints :: NumericConstraint -> NumericConstraint -> NumericConstraint
combineNumericConstraints leftConstraint rightConstraint =
  case (leftConstraint, rightConstraint) of
    (IntegralLiteralNumericConstraint leftRange, IntegralLiteralNumericConstraint rightRange) ->
      IntegralLiteralNumericConstraint (combineIntegerLiteralRanges leftRange rightRange)
    (IntegralLiteralNumericConstraint literalRange, _) ->
      IntegralLiteralNumericConstraint literalRange
    (_, IntegralLiteralNumericConstraint literalRange) ->
      IntegralLiteralNumericConstraint literalRange
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
      | typeSatisfiesNumericConstraint numericConstraint replacementType ->
          Just replacementType
      | otherwise ->
          Nothing

constrainNumericOperatorType :: NumericConstraint -> ExpressionType -> InferState -> Maybe InferState
constrainNumericOperatorType numericConstraint expressionType state =
  case resolveType state expressionType of
    TVarType typeVar ->
      Just (addNumericTypeVarConstraint typeVar numericConstraint state)
    resolvedType
      | typeSatisfiesNumericConstraint numericConstraint resolvedType ->
          Just state
      | otherwise ->
          Nothing

typeSatisfiesNumericConstraint :: NumericConstraint -> ExpressionType -> Bool
typeSatisfiesNumericConstraint numericConstraint expressionType =
  case numericConstraint of
    AnyNumericConstraint ->
      case expressionType of
        TIntType -> True
        TIntegerLiteralType {} -> True
        TFloatType -> True
        TNumericType {} -> True
        TVarType {} -> True
        _ -> False
    RuntimeArithmeticNumericConstraint ->
      case expressionType of
        TIntType -> True
        TIntegerLiteralType {} -> True
        TFloatType -> True
        TNumericType numericType ->
          numericTypeSupportsRuntimeArithmetic numericType
        TVarType {} -> True
        _ -> False
    RuntimeComparisonNumericConstraint ->
      case expressionType of
        TIntType -> True
        TIntegerLiteralType {} -> True
        TFloatType -> True
        TNumericType numericType ->
          numericTypeSupportsRuntimeComparison numericType
        TVarType {} -> True
        _ -> False
    IntegralNumericConstraint ->
      case expressionType of
        TIntType -> True
        TIntegerLiteralType {} -> True
        TNumericType numericType -> numericTypeIsIntegral numericType
        TVarType {} -> True
        _ -> False
    IntegralLiteralNumericConstraint literalRange ->
      case expressionType of
        TIntType -> True
        TIntegerLiteralType {} -> True
        TNumericType numericType ->
          numericTypeIsIntegral numericType && integerLiteralRangeFitsNumericType literalRange numericType
        TVarType {} -> True
        _ -> False

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

mkNumericBinaryTypeError :: Text -> NumericRuleResult -> ExpressionType -> ExpressionType -> Diagnostic
mkNumericBinaryTypeError operatorSymbol _ leftType rightType =
  mkBinaryTypeError operatorSymbol leftType rightType

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
        <> "' is only supported for Bool, integral numeric, Float/Float16/Float32/Float64, lists and tuples containing equality-supported elements, and ADTs containing equality-supported constructor payloads, found "
        <> renderType foundType
        <> callableEqualityUnsupportedNote foundType
    )

callableEqualityUnsupportedNote :: ExpressionType -> Text
callableEqualityUnsupportedNote expressionType
  | typeContainsFunction expressionType =
      "; callable values are not equality-supported"
  | otherwise = ""

typeContainsFunction :: ExpressionType -> Bool
typeContainsFunction expressionType =
  case expressionType of
    TFunctionType {} -> True
    TListType elementType -> typeContainsFunction elementType
    TTupleType elementTypes -> any typeContainsFunction elementTypes
    TDataType _ typeArguments -> any typeContainsFunction typeArguments
    _ -> False

mkDuplicateDataTypeDeclarationError :: Text -> SourceSpan -> Diagnostic
mkDuplicateDataTypeDeclarationError typeName spanValue =
  setDiagnosticSubject typeName $
    setDiagnosticPrimarySpan spanValue $
      mkDiagnostic
        "E2014"
        ("duplicate data type declaration '" <> typeName <> "'")

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

mkExplicitTypeApplicationTargetError :: Diagnostic
mkExplicitTypeApplicationTargetError =
  mkDiagnostic
    "E2017"
    "explicit type application target must be a generalized binding"

mkNumericConversionLiteralTypeError :: Text -> Integer -> NumericType -> (Integer, Integer) -> Diagnostic
mkNumericConversionLiteralTypeError conversionName literalValue targetType (lowerBound, upperBound) =
  mkDiagnostic
    "E2006"
    ( "numeric conversion '"
        <> conversionName
        <> "' cannot convert integer literal "
        <> Text.pack (show literalValue)
        <> " outside "
        <> renderNumericTypeName targetType
        <> " range "
        <> Text.pack (show lowerBound)
        <> ".."
        <> Text.pack (show upperBound)
    )

mkNumericConversionFractionalLiteralTypeError :: Text -> Double -> NumericType -> (Integer, Integer) -> Diagnostic
mkNumericConversionFractionalLiteralTypeError conversionName literalValue targetType (lowerBound, upperBound) =
  mkDiagnostic
    "E2006"
    ( "numeric conversion '"
        <> conversionName
        <> "' cannot convert fractional literal "
        <> Text.pack (show literalValue)
        <> " to integral target "
        <> renderNumericTypeName targetType
        <> "; expected a finite integral value in range "
        <> Text.pack (show lowerBound)
        <> ".."
        <> Text.pack (show upperBound)
    )

mkNumericConversionFloatLiteralOverflowError :: Text -> Double -> NumericType -> Double -> Diagnostic
mkNumericConversionFloatLiteralOverflowError conversionName literalValue targetType maxMagnitude =
  mkDiagnostic
    "E2006"
    ( "numeric conversion '"
        <> conversionName
        <> "' cannot convert fractional literal "
        <> Text.pack (show literalValue)
        <> " outside finite "
        <> renderNumericTypeName targetType
        <> " magnitude "
        <> Text.pack (show maxMagnitude)
    )

mkTargetedFractionalLiteralOverflowError :: Double -> NumericType -> Double -> Diagnostic
mkTargetedFractionalLiteralOverflowError literalValue targetType maxMagnitude =
  mkDiagnostic
    "E2006"
    ( "fractional literal "
        <> Text.pack (show literalValue)
        <> " cannot target finite "
        <> renderNumericTypeName targetType
        <> " magnitude "
        <> Text.pack (show maxMagnitude)
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
  mkDiagnostic "E2003" ("builtin operator '" <> operatorSymbol <> "' has no value type rule")

mkNumericSectionOperandTypeError :: Text -> NumericRuleResult -> ExpressionType -> Diagnostic
mkNumericSectionOperandTypeError operatorSymbol _ operandType =
  mkDiagnostic
    "E2003"
    ( "operator section '"
        <> operatorSymbol
        <> "' requires a numeric operand, found "
        <> renderType operandType
    )

mkTypeSchemeNumericConstraintError :: NumericConstraint -> ExpressionType -> Diagnostic
mkTypeSchemeNumericConstraintError _ foundType =
  mkDiagnostic
    "E2003"
    ("primitive numeric constraint cannot be satisfied by " <> renderType foundType)

mkTypeSchemeStrictEqualityConstraintError :: ExpressionType -> Diagnostic
mkTypeSchemeStrictEqualityConstraintError foundType =
  mkDiagnostic
    "E2004"
    ("primitive strict equality constraint cannot be satisfied by " <> renderType foundType)

mkMissingOperatorBindingError :: Text -> Diagnostic
mkMissingOperatorBindingError operatorSymbol =
  mkDiagnostic "E2010" ("operator '" <> operatorSymbol <> "' has no executable binding")

mkMissingClassMethodError :: Text -> Diagnostic
mkMissingClassMethodError methodKey =
  setDiagnosticSubject methodKey $
    mkDiagnostic "E2015" ("missing class method '" <> methodKey <> "'")

mkMissingImplMethodBodyError :: Text -> Diagnostic
mkMissingImplMethodBodyError methodKey =
  setDiagnosticSubject methodKey $
    mkDiagnostic "E2015" ("missing impl method body '" <> methodKey <> "'")

mkAmbiguousQualifiedMethodBodyError :: Text -> Diagnostic
mkAmbiguousQualifiedMethodBodyError methodKey =
  setDiagnosticSubject methodKey $
    mkDiagnostic "E2015" ("ambiguous qualified method body '" <> methodKey <> "'")

mkNoMatchingQualifiedMethodBodyError :: Text -> [ExpressionType] -> Diagnostic
mkNoMatchingQualifiedMethodBodyError methodKey argumentTypes =
  setDiagnosticSubject methodKey $
    mkDiagnostic
      "E2015"
      ( "no matching qualified method body '"
          <> methodKey
          <> "' for argument types "
          <> renderArgumentTypeList argumentTypes
      )

mkAmbiguousQualifiedMethodBodyForArgumentsError :: Text -> [ExpressionType] -> Diagnostic
mkAmbiguousQualifiedMethodBodyForArgumentsError methodKey argumentTypes =
  setDiagnosticSubject methodKey $
    mkDiagnostic
      "E2015"
      ( "ambiguous qualified method body '"
          <> methodKey
          <> "' for argument types "
          <> renderArgumentTypeList argumentTypes
      )

renderArgumentTypeList :: [ExpressionType] -> Text
renderArgumentTypeList argumentTypes =
  Text.intercalate ", " (map renderType argumentTypes)

mkInvalidQualifiedMethodSignatureError :: Text -> SignaturePayload -> Diagnostic
mkInvalidQualifiedMethodSignatureError methodKey methodSignature =
  setDiagnosticSubject methodKey $
    mkDiagnostic
      "E2015"
      ( "invalid or unsupported class method signature for '"
          <> methodKey
          <> "': '"
          <> renderSignaturePayload methodSignature
          <> "'"
      )

mkImplMethodMissingClassMethodError :: Text -> SourceSpan -> Diagnostic
mkImplMethodMissingClassMethodError methodKey methodSpan =
  setDiagnosticSubject methodKey $
    setDiagnosticPrimarySpan methodSpan $
      mkDiagnostic
        "E2015"
        ("class method metadata for '" <> methodKey <> "' must be declared before impl method body")

mkImplMethodTypeMismatchError :: Text -> SourceSpan -> ExpressionType -> ExpressionType -> Diagnostic
mkImplMethodTypeMismatchError methodKey methodSpan declaredType inferredType =
  setDiagnosticSubject methodKey $
    setDiagnosticPrimarySpan methodSpan $
      mkDiagnostic
        "E2016"
        ( "impl method '"
            <> methodKey
            <> "' declared as "
            <> renderType declaredType
            <> " but inferred as "
            <> renderType inferredType
        )

mkUnknownConstructorPayloadTypeError :: Identifier -> Diagnostic
mkUnknownConstructorPayloadTypeError payloadTypeName =
  mkDiagnostic
    "E2013"
    ("unknown constructor payload type '" <> identifierText payloadTypeName <> "' in generic data declaration")

mkMissingConstructorTypeParameterBindingError :: Text -> Diagnostic
mkMissingConstructorTypeParameterBindingError parameterName =
  mkDiagnostic
    "E2013"
    ("internal constructor scheme error: missing binding for type parameter '" <> parameterName <> "'")

mkMissingExplicitConstraintClassError :: Text -> Diagnostic
mkMissingExplicitConstraintClassError constraintName =
  mkDiagnostic "E2009" ("missing class declaration '" <> constraintName <> "'")

mkExplicitConstraintArityError :: Text -> Int -> Diagnostic
mkExplicitConstraintArityError constraintName expectedArity =
  mkDiagnostic
    "E2009"
    ( "constraint '"
        <> constraintName
        <> "' expects "
        <> Text.pack (show expectedArity)
        <> " argument(s), got 1"
    )

mkMissingExplicitConstraintImplFactError :: Text -> Diagnostic
mkMissingExplicitConstraintImplFactError implFactKey =
  mkDiagnostic "E2009" ("missing impl fact '" <> implFactKey <> "'")

mkAmbiguousExplicitConstraintError :: Text -> ExpressionType -> Diagnostic
mkAmbiguousExplicitConstraintError constraintName argumentType =
  mkDiagnostic
    "E2009"
    ( "ambiguous/defaulting explicit constraint '"
        <> constraintName
        <> "("
        <> renderType argumentType
        <> ")': explicit constrained signatures do not default unresolved type variables"
    )

mkAmbiguousInferredConstraintError :: Text -> ExpressionType -> Diagnostic
mkAmbiguousInferredConstraintError constraintName argumentType =
  mkDiagnostic
    "E2009"
    ( "ambiguous/defaulting inferred constraint '"
        <> constraintName
        <> "("
        <> renderType argumentType
        <> ")': inferred class constraints do not default unresolved type variables"
    )

mkAmbiguousDeferredConstraintError :: Bool -> Text -> ExpressionType -> Diagnostic
mkAmbiguousDeferredConstraintError inferredConstraint =
  if inferredConstraint
    then mkAmbiguousInferredConstraintError
    else mkAmbiguousExplicitConstraintError

mkPatternTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkPatternTypeMismatchError scrutineeType patternType =
  mkDiagnostic
    "E2011"
    ( "case pattern of type "
        <> renderType patternType
        <> " does not match scrutinee type "
        <> renderType scrutineeType
    )

mkListPatternTypeMismatchError :: ExpressionType -> Diagnostic
mkListPatternTypeMismatchError scrutineeType =
  mkDiagnostic
    "E2011"
    ("case pattern of list type does not match scrutinee type " <> renderType scrutineeType)

mkTuplePatternTypeMismatchError :: ExpressionType -> Diagnostic
mkTuplePatternTypeMismatchError scrutineeType =
  mkDiagnostic
    "E2011"
    ("tuple case pattern does not match scrutinee type " <> renderType scrutineeType)

mkTuplePatternArityMismatchError :: Int -> Int -> Diagnostic
mkTuplePatternArityMismatchError patternArity scrutineeArity =
  mkDiagnostic
    "E2011"
    ( "tuple case pattern expects "
        <> Text.pack (show patternArity)
        <> " element(s), found "
        <> Text.pack (show scrutineeArity)
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

mkInvalidSignatureTypeError :: InferState -> Text -> SourceSpan -> SignaturePayload -> Diagnostic
mkInvalidSignatureTypeError state symbol signatureSpan signaturePayload =
  setDiagnosticSubject symbol $
    setDiagnosticPrimarySpan
      signatureSpan
      ( mkDiagnostic
          "E2009"
          (invalidSignatureSummary state symbol signaturePayload)
      )

invalidSignatureSummary :: InferState -> Text -> SignaturePayload -> Text
invalidSignatureSummary state symbol signaturePayload =
  case signaturePayload of
    ConstrainedSignature constraints _
      | Just duplicateName <- duplicateConstraintName constraints ->
          "invalid or unsupported signature for '"
            <> symbol
            <> "': duplicate constraint '"
            <> duplicateName
            <> "' in '"
            <> renderSignaturePayload signaturePayload
            <> "'"
    ConstrainedSignature constraints signatureType
      | constrainedSignatureHasTypeVariable constraints signatureType ->
          "invalid or unsupported signature for '"
            <> symbol
            <> "': type-variable constrained signatures require every constrained variable to appear in the signature body before inference can accept '"
            <> renderSignaturePayload signaturePayload
            <> "'"
    ConstrainedSignature constraints _
      | Just reason <- concreteConstraintFailureSummary state constraints ->
          "invalid or unsupported signature for '"
            <> symbol
            <> "': "
            <> reason
            <> " in '"
            <> renderSignaturePayload signaturePayload
            <> "'"
    _ ->
      "invalid or unsupported signature for '"
        <> symbol
        <> "': '"
        <> renderSignaturePayload signaturePayload
        <> "'"

concreteConstraintFailureSummary :: InferState -> [SignatureConstraint] -> Maybe Text
concreteConstraintFailureSummary state constraints
  | null constraints = Nothing
  | otherwise = firstJust (map constraintFailureSummary constraints)
  where
    constraintFailureSummary (SignatureConstraint constraintName arguments)
      | Nothing <- maybeClassArity =
          Just ("missing class declaration '" <> constraintNameText <> "'")
      | Just expectedArity <- maybeClassArity,
        expectedArity /= length arguments =
          Just
            ( "constraint '"
                <> constraintNameText
                <> "' expects "
                <> Text.pack (show expectedArity)
                <> " argument(s), got "
                <> Text.pack (show (length arguments))
            )
      | [argument] <- arguments,
        concreteConstraintArgument argument,
        let implFactKey = constraintImplFactKey constraintName argument,
        Set.notMember implFactKey (inferConcreteImplFacts state) =
          Just ("missing impl fact '" <> implFactKey <> "'")
      | otherwise =
          Nothing
      where
        constraintNameText = identifierText constraintName
        maybeClassArity = Map.lookup constraintNameText (inferClassFacts state)

    firstJust results =
      case results of
        [] -> Nothing
        Just result : _ -> Just result
        Nothing : rest -> firstJust rest

constrainedSignatureHasTypeVariable :: [SignatureConstraint] -> ConstraintSignatureType -> Bool
constrainedSignatureHasTypeVariable constraints signatureType =
  any constraintHasTypeVariable constraints
    || constraintTypeHasTypeVariable signatureType

constraintHasTypeVariable :: SignatureConstraint -> Bool
constraintHasTypeVariable (SignatureConstraint _ arguments) =
  any constraintTypeHasTypeVariable arguments

constraintTypeHasTypeVariable :: ConstraintSignatureType -> Bool
constraintTypeHasTypeVariable signatureType =
  case signatureType of
    ConstraintTypeName name ->
      identifierLooksLikeTypeVariable name
    ConstraintTypeApplication name arguments ->
      identifierLooksLikeTypeVariable name || any constraintTypeHasTypeVariable arguments
    ConstraintTypeList innerType ->
      constraintTypeHasTypeVariable innerType
    ConstraintTypeTuple elementTypes ->
      any constraintTypeHasTypeVariable elementTypes
    ConstraintTypeFunction argumentType resultType ->
      constraintTypeHasTypeVariable argumentType || constraintTypeHasTypeVariable resultType

duplicateConstraintName :: [SignatureConstraint] -> Maybe Text
duplicateConstraintName constraints =
  go Set.empty constraints
  where
    go seen remainingConstraints =
      case remainingConstraints of
        [] -> Nothing
        SignatureConstraint constraintName _ : rest ->
          let constraintNameText = identifierText constraintName
           in if Set.member constraintNameText seen
                then Just constraintNameText
                else go (Set.insert constraintNameText seen) rest

mkIfConditionTypeError :: ExpressionType -> Diagnostic
mkIfConditionTypeError foundType =
  mkDiagnostic "E2001" ("if condition must have type Bool, found " <> renderType foundType)

mkCaseGuardTypeError :: ExpressionType -> Diagnostic
mkCaseGuardTypeError foundType =
  mkDiagnostic "E2001" ("case guard must have type Bool, found " <> renderType foundType)

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
    TIntegerLiteralType {} -> "Int"
    TFloatType -> "Float"
    TNumericType numericType -> renderNumericTypeName numericType
    TBoolType -> "Bool"
    TListType elementType -> "[" <> renderType elementType <> "]"
    TTupleType elementTypes -> "(" <> Text.intercalate ", " (map renderType elementTypes) <> ")"
    TDataType typeName typeArguments
      | null typeArguments -> identifierText typeName
      | otherwise ->
          identifierText typeName
            <> "<"
            <> Text.intercalate ", " (map renderType typeArguments)
            <> ">"
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
    PConsList headPattern tailPattern ->
      extendBoundWithPattern tailPattern (extendBoundWithPattern headPattern bound)
    PTuple patterns ->
      foldl' (flip extendBoundWithPattern) bound patterns
    PAs name pattern ->
      extendBoundWithPattern pattern (Set.insert (identifierText name) bound)
    POr alternatives ->
      Set.union bound (commonPatternBinderNames alternatives)

commonPatternBinderNames :: [Pattern] -> Set Text
commonPatternBinderNames alternatives =
  case alternatives of
    [] -> Set.empty
    firstAlternative : rest ->
      foldl'
        Set.intersection
        (patternBinderNames firstAlternative)
        (map patternBinderNames rest)

patternBinderNames :: Pattern -> Set Text
patternBinderNames pattern =
  case pattern of
    PVariable name -> Set.singleton (identifierText name)
    PWildcard -> Set.empty
    PLiteral {} -> Set.empty
    PConstructor _ patterns ->
      Set.unions (map patternBinderNames patterns)
    PList patterns ->
      Set.unions (map patternBinderNames patterns)
    PConsList headPattern tailPattern ->
      Set.union (patternBinderNames headPattern) (patternBinderNames tailPattern)
    PTuple patterns ->
      Set.unions (map patternBinderNames patterns)
    PAs name nestedPattern ->
      Set.insert (identifierText name) (patternBinderNames nestedPattern)
    POr alternatives ->
      commonPatternBinderNames alternatives

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
    step (maybeExpectedBodyType, stateAcc) (CaseArm pattern guardExpr bodyExpr) =
      let (rawPatternTyping, stateAfterPatternCheck) =
            inferPatternType env scrutineeType pattern stateAcc
          (patternTyping, stateAfterPattern) =
            rejectDuplicatePatternBinders pattern rawPatternTyping stateAcc stateAfterPatternCheck
       in
        if patternSkipsBranchType patternTyping
          then (maybeExpectedBodyType, stateAfterPattern)
          else
            let armEnv =
                  patternBindings patternTyping `Map.union` env
                stateAfterGuard =
                  inferCaseGuardType builtinMode armEnv stateAfterPattern guardExpr
                (maybeBodyType, stateAfterBody) =
                  inferExprType builtinMode armEnv stateAfterGuard bodyExpr
             in
              case (maybeExpectedBodyType, maybeBodyType) of
                (Nothing, _) ->
                  (fmap (resolveType stateAfterBody) maybeBodyType, stateAfterBody)
                (expectedBodyType, Nothing) ->
                  (expectedBodyType, stateAfterBody)
                (Just inferredExpectedBodyType, Just inferredBodyType) ->
                  case unifyTypes inferredExpectedBodyType inferredBodyType stateAfterBody of
                    Just unifiedState ->
                      (Just (mergedUnifiedType unifiedState inferredExpectedBodyType inferredBodyType), unifiedState)
                    Nothing ->
                      ( Just inferredExpectedBodyType,
                        addTypeError
                          stateAfterBody
                          ( mkPatternBranchTypeMismatchError
                              (resolveType stateAfterBody inferredExpectedBodyType)
                              (resolveType stateAfterBody inferredBodyType)
                          )
                      )

    inferCaseGuardType ::
      BuiltinResolutionMode ->
      TypeEnv ->
      InferState ->
      Maybe Expr ->
      InferState
    inferCaseGuardType builtinMode' armEnv stateAcc guardExpr =
      case guardExpr of
        Nothing -> stateAcc
        Just conditionExpr ->
          let (maybeGuardType, stateAfterGuard) =
                inferExprType builtinMode' armEnv stateAcc conditionExpr
           in case maybeGuardType of
                Just inferredGuardType ->
                  case unifyTypes inferredGuardType TBoolType stateAfterGuard of
                    Just unifiedState -> unifiedState
                    Nothing ->
                      addTypeError
                        stateAfterGuard
                        (mkCaseGuardTypeError (resolveType stateAfterGuard inferredGuardType))
                Nothing ->
                  stateAfterGuard

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

rejectDuplicatePatternBinders :: Pattern -> PatternTyping -> InferState -> InferState -> (PatternTyping, InferState)
rejectDuplicatePatternBinders pattern typing stableState checkedState =
  case patternDuplicateBinderNames pattern of
    [] -> (typing, checkedState)
    duplicateNames ->
      let stateWithDuplicateErrors =
            foldl' addDuplicateError checkedState duplicateNames
       in
        ( typing {patternSkipsBranchType = True},
          rollbackSkippedPatternState stableState stateWithDuplicateErrors
        )
  where
    addDuplicateError stateAcc duplicateName =
      addTypeError stateAcc (mkDuplicatePatternBinderError duplicateName)

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
        PConsList headPattern tailPattern ->
          collectNested seen duplicatesAcc [headPattern, tailPattern]
        PTuple nestedPatterns ->
          collectNested seen duplicatesAcc nestedPatterns
        PAs name nestedPattern ->
          let nameText = identifierText name
              (seenAfterName, duplicatesAfterName) =
                if Set.member nameText seen
                  then (seen, Set.insert nameText duplicatesAcc)
                  else (Set.insert nameText seen, duplicatesAcc)
           in collect nestedPattern seenAfterName duplicatesAfterName
        POr alternatives ->
          let duplicatesAfterAlternatives =
                foldl'
                  ( \duplicatesAcc' alternative ->
                      Set.union duplicatesAcc' (Set.intersection seen (patternBinderNames alternative))
                  )
                  duplicatesAcc
                  alternatives
           in (Set.union seen (commonPatternBinderNames alternatives), duplicatesAfterAlternatives)

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
    PConsList headPattern tailPattern ->
      inferConsListPatternType env scrutineeType headPattern tailPattern state
    PTuple patterns ->
      inferTuplePatternType env scrutineeType patterns state
    PAs name pattern ->
      let (typing, stateAfterPattern) =
            inferPatternType env scrutineeType pattern state
       in
        if patternSkipsBranchType typing
          then (typing, stateAfterPattern)
          else
            ( typing
                { patternBindings =
                    Map.insert
                      (identifierText name)
                      (PlainTypeBinding (resolveType stateAfterPattern scrutineeType))
                      (patternBindings typing)
                },
              stateAfterPattern
            )
    POr alternatives ->
      inferOrPatternType env scrutineeType alternatives state

inferOrPatternType ::
  TypeEnv ->
  ExpressionType ->
  [Pattern] ->
  InferState ->
  (PatternTyping, InferState)
inferOrPatternType env scrutineeType alternatives initialState =
  case alternatives of
    [] ->
      ( skipBranchPatternTyping,
        addTypeError initialState mkEmptyOrPatternError
      )
    firstAlternative : rest ->
      let (firstTyping, stateAfterFirst) =
            inferOrPatternAlternative firstAlternative initialState
       in
        if patternSkipsBranchType firstTyping
          then (firstTyping, rollbackSkippedPatternState initialState stateAfterFirst)
          else
            let expectedBinderNames = Map.keysSet (patternBindings firstTyping)
             in inferRemainingAlternatives
                  expectedBinderNames
                  (patternBindings firstTyping)
                  stateAfterFirst
                  rest
  where
    inferOrPatternAlternative alternativePattern stateAcc =
      let (rawTyping, stateAfterPatternCheck) =
            inferPatternType env scrutineeType alternativePattern stateAcc
       in rejectDuplicatePatternBinders
            alternativePattern
            rawTyping
            stateAcc
            stateAfterPatternCheck

    inferRemainingAlternatives expectedBinderNames bindingsAcc stateAcc remainingAlternatives =
      case remainingAlternatives of
        [] ->
          ( emptyPatternTyping
              {patternBindings = resolvePatternBindings stateAcc bindingsAcc},
            stateAcc
          )
        alternativePattern : restAlternatives ->
          let (alternativeTyping, stateAfterAlternative) =
                inferOrPatternAlternative alternativePattern stateAcc
           in
            if patternSkipsBranchType alternativeTyping
              then (alternativeTyping, rollbackSkippedPatternState initialState stateAfterAlternative)
              else
                let alternativeBindings = patternBindings alternativeTyping
                    alternativeBinderNames = Map.keysSet alternativeBindings
                 in
                  if alternativeBinderNames /= expectedBinderNames
                    then
                      ( skipBranchPatternTyping,
                        rollbackSkippedPatternState
                          initialState
                          ( addTypeError
                              stateAfterAlternative
                              (mkOrPatternBinderSetMismatchError expectedBinderNames alternativeBinderNames)
                          )
                      )
                    else
                      case unifyOrPatternBinders bindingsAcc alternativeBindings stateAfterAlternative of
                        Left failedState ->
                          (skipBranchPatternTyping, rollbackSkippedPatternState initialState failedState)
                        Right (mergedBindings, stateAfterBinders) ->
                          inferRemainingAlternatives
                            expectedBinderNames
                            mergedBindings
                            stateAfterBinders
                            restAlternatives

    unifyOrPatternBinders bindingsAcc alternativeBindings stateAcc =
      foldl'
        unifyBinder
        (Right (bindingsAcc, stateAcc))
        (Set.toList (Map.keysSet bindingsAcc))
      where
        unifyBinder maybeAcc binderName =
          case maybeAcc of
            Left failedState -> Left failedState
            Right (mergedBindings, stateForBinder) ->
              case (Map.lookup binderName mergedBindings, Map.lookup binderName alternativeBindings) of
                (Just leftBinding, Just rightBinding) ->
                  let leftType = patternBindingExpressionType leftBinding
                      rightType = patternBindingExpressionType rightBinding
                   in case unifyTypes leftType rightType stateForBinder of
                        Just unifiedState ->
                          Right
                            ( Map.insert
                                binderName
                                (PlainTypeBinding (resolveType unifiedState leftType))
                                mergedBindings,
                              unifiedState
                            )
                        Nothing ->
                          Left
                            ( addTypeError
                                stateForBinder
                                ( mkOrPatternBinderTypeMismatchError
                                    binderName
                                    (resolveType stateForBinder leftType)
                                    (resolveType stateForBinder rightType)
                                )
                            )
                _ ->
                  Left
                    ( addTypeError
                        stateForBinder
                        (mkOrPatternBinderSetMismatchError (Map.keysSet mergedBindings) (Map.keysSet alternativeBindings))
                    )

patternBindingExpressionType :: TypeBinding -> ExpressionType
patternBindingExpressionType binding =
  case binding of
    PlainTypeBinding expressionType -> expressionType
    _ -> error "internal type inference error: non-plain case pattern binding"

resolvePatternBindings :: InferState -> TypeEnv -> TypeEnv
resolvePatternBindings state bindings =
  Map.map resolvePatternBinding bindings
  where
    resolvePatternBinding binding =
      case binding of
        PlainTypeBinding expressionType ->
          PlainTypeBinding (resolveType state expressionType)
        _ -> binding

inferConstructorPatternType ::
  TypeEnv ->
  ExpressionType ->
  Identifier ->
  [Pattern] ->
  InferState ->
  (PatternTyping, InferState)
inferConstructorPatternType env scrutineeType constructorName patterns state =
  case Map.lookup constructorNameText env of
    Just constructorBinding ->
      case instantiateConstructorBinding constructorBinding state of
        Just (argumentTypes, constructorResultType, stateAfterConstructor) ->
          let expectedArity = length argumentTypes
           in
            if expectedArity /= length patterns
              then
                ( skipBranchPatternTyping,
                  addTypeError
                    stateAfterConstructor
                    (mkConstructorPatternArityError constructorNameText expectedArity (length patterns))
                )
              else
                case unifyTypes scrutineeType constructorResultType stateAfterConstructor of
                  Just stateAfterResultCheck ->
                    inferConstructorArgumentPatterns
                      env
                      (map (resolveType stateAfterResultCheck) argumentTypes)
                      patterns
                      stateAfterResultCheck
                  Nothing ->
                    ( skipBranchPatternTyping,
                      addTypeError
                        stateAfterConstructor
                        ( mkPatternTypeMismatchError
                            (resolveType stateAfterConstructor scrutineeType)
                            constructorResultType
                        )
                    )
        Nothing ->
          ( skipBranchPatternTyping,
            addTypeError
              state
              (mkUnknownConstructorPatternError constructorNameText)
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
              ( mkListPatternTypeMismatchError
                  (resolveType stateWithElementType scrutineeType)
              )
   in
    if hasNewPatternError stateWithElementType stateAfterListCheck
      then (skipBranchPatternTyping, rollbackSkippedPatternState state stateAfterListCheck)
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

inferConsListPatternType ::
  TypeEnv ->
  ExpressionType ->
  Pattern ->
  Pattern ->
  InferState ->
  (PatternTyping, InferState)
inferConsListPatternType env scrutineeType headPattern tailPattern state =
  let (elementType, stateWithElementType) = freshTypeVar state
      listPatternType = TListType elementType
      stateAfterListCheck =
        case unifyTypes scrutineeType listPatternType stateWithElementType of
          Just unifiedState -> unifiedState
          Nothing ->
            addTypeError
              stateWithElementType
              ( mkListPatternTypeMismatchError
                  (resolveType stateWithElementType scrutineeType)
              )
   in
    if hasNewPatternError stateWithElementType stateAfterListCheck
      then (skipBranchPatternTyping, rollbackSkippedPatternState state stateAfterListCheck)
      else
        inferConsListSubpatterns
          env
          (resolveType stateAfterListCheck elementType)
          headPattern
          tailPattern
          stateAfterListCheck

inferConsListSubpatterns ::
  TypeEnv ->
  ExpressionType ->
  Pattern ->
  Pattern ->
  InferState ->
  (PatternTyping, InferState)
inferConsListSubpatterns env elementType headPattern tailPattern initialState =
  let (headTyping, stateAfterHeadPattern) =
        inferPatternType env elementType headPattern initialState
   in
    if patternSkipsBranchType headTyping
      then (headTyping, rollbackSkippedPatternState initialState stateAfterHeadPattern)
      else
        let tailListType = TListType (resolveType stateAfterHeadPattern elementType)
            (tailTyping, stateAfterTailPattern) =
              inferPatternType env tailListType tailPattern stateAfterHeadPattern
            mergedTyping = mergePatternTyping tailTyping headTyping
         in
          if patternSkipsBranchType mergedTyping
            then (mergedTyping, rollbackSkippedPatternState initialState stateAfterTailPattern)
            else (mergedTyping, stateAfterTailPattern)

inferTuplePatternType ::
  TypeEnv ->
  ExpressionType ->
  [Pattern] ->
  InferState ->
  (PatternTyping, InferState)
inferTuplePatternType env scrutineeType patterns state =
  case resolveType state scrutineeType of
    TTupleType elementTypes
      | length elementTypes == length patterns ->
          inferConstructorArgumentPatterns env elementTypes patterns state
      | otherwise ->
          ( skipBranchPatternTyping,
            addTypeError
              state
              (mkTuplePatternArityMismatchError (length patterns) (length elementTypes))
          )
    resolvedScrutineeType ->
      let (elementTypes, stateWithElementTypes) =
            freshTypeVars (length patterns) state
          tuplePatternType = TTupleType elementTypes
          stateAfterTupleCheck =
            case unifyTypes scrutineeType tuplePatternType stateWithElementTypes of
              Just unifiedState -> unifiedState
              Nothing ->
                addTypeError
                  stateWithElementTypes
                  (mkTuplePatternTypeMismatchError resolvedScrutineeType)
       in
        if hasNewPatternError stateWithElementTypes stateAfterTupleCheck
          then (skipBranchPatternTyping, rollbackSkippedPatternState state stateAfterTupleCheck)
          else
            inferConstructorArgumentPatterns
              env
              (map (resolveType stateAfterTupleCheck) elementTypes)
              patterns
              stateAfterTupleCheck
  where
    freshTypeVars count initialState =
      go [] initialState count

    go reversedTypes stateAcc remainingCount
      | remainingCount <= 0 = (reverse reversedTypes, stateAcc)
      | otherwise =
          let (nextType, nextState) = freshTypeVar stateAcc
           in go (nextType : reversedTypes) nextState (remainingCount - 1)

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

mkEmptyOrPatternError :: Diagnostic
mkEmptyOrPatternError =
  mkDiagnostic
    "E2011"
    "or-pattern must contain at least one alternative"

mkOrPatternBinderSetMismatchError :: Set Text -> Set Text -> Diagnostic
mkOrPatternBinderSetMismatchError expectedNames foundNames =
  mkDiagnostic
    "E2011"
    ( "or-pattern alternatives must bind the same names, expected "
        <> renderBinderSet expectedNames
        <> " but found "
        <> renderBinderSet foundNames
    )

mkOrPatternBinderTypeMismatchError :: Text -> ExpressionType -> ExpressionType -> Diagnostic
mkOrPatternBinderTypeMismatchError binderName leftType rightType =
  mkDiagnostic
    "E2011"
    ( "or-pattern binder '"
        <> binderName
        <> "' has incompatible types "
        <> renderType leftType
        <> " and "
        <> renderType rightType
    )

renderBinderSet :: Set Text -> Text
renderBinderSet names =
  "{" <> Text.intercalate ", " (Set.toList names) <> "}"

supportsRuntimeEqualityType :: InferState -> ExpressionType -> Bool
supportsRuntimeEqualityType state expressionType =
  supportsRuntimeEqualityTypeWith Set.empty state expressionType

supportsRuntimeEqualityTypeWith :: Set Text -> InferState -> ExpressionType -> Bool
supportsRuntimeEqualityTypeWith seenDataTypes state expressionType =
  -- Keep compile-time acceptance aligned with the currently implemented
  -- runtime equality evaluator to avoid compile/runtime contract drift.
  case resolveType state expressionType of
    TIntType -> True
    TIntegerLiteralType {} -> True
    TFloatType -> True
    TNumericType numericType -> numericTypeSupportsRuntimeComparison numericType
    TBoolType -> True
    TListType elementType -> supportsRuntimeEqualityTypeWith seenDataTypes state elementType
    TTupleType elementTypes -> all (supportsRuntimeEqualityTypeWith seenDataTypes state) elementTypes
    TDataType typeName typeArguments ->
      dataTypeSupportsRuntimeEqualityWith seenDataTypes state typeName typeArguments
    _ -> False

dataTypeSupportsRuntimeEquality :: InferState -> Identifier -> [ExpressionType] -> Bool
dataTypeSupportsRuntimeEquality state typeName typeArguments =
  dataTypeSupportsRuntimeEqualityWith Set.empty state typeName typeArguments

dataTypeSupportsRuntimeEqualityWith :: Set Text -> InferState -> Identifier -> [ExpressionType] -> Bool
dataTypeSupportsRuntimeEqualityWith seenDataTypes state typeName typeArguments =
  let resolvedTypeArguments = map (resolveType state) typeArguments
      dataTypeKey =
        identifierText typeName
          <> "<"
          <> Text.intercalate ", " (map renderType resolvedTypeArguments)
          <> ">"
   in if Set.member dataTypeKey seenDataTypes
        then True
        else dataTypeSupportsRuntimeEqualityUnseen (Set.insert dataTypeKey seenDataTypes) resolvedTypeArguments
  where
    dataTypeSupportsRuntimeEqualityUnseen nextSeenDataTypes resolvedTypeArguments =
      case Map.lookup (identifierText typeName) (inferDataTypes state) of
        Just (DataTypeBinding typeParameters constructors)
          | length typeParameters == length resolvedTypeArguments ->
              let typeParameterBindings =
                    Map.fromList
                      (zip (map identifierText typeParameters) resolvedTypeArguments)
               in all
                    (all (constructorArgumentSupportsRuntimeEquality nextSeenDataTypes typeParameterBindings))
                    constructors
        _ -> False

    constructorArgumentSupportsRuntimeEquality nextSeenDataTypes typeParameterBindings argumentType =
      case argumentType of
        ConstructorArgumentMonomorphic expressionType ->
          supportsRuntimeEqualityTypeWith nextSeenDataTypes state expressionType
        ConstructorArgumentParameter parameterName ->
          case Map.lookup parameterName typeParameterBindings of
            Just expressionType -> supportsRuntimeEqualityTypeWith nextSeenDataTypes state expressionType
            Nothing -> False
        ConstructorArgumentFresh -> False

supportsDeferredEqualityOperandType :: InferState -> ExpressionType -> Bool
supportsDeferredEqualityOperandType state expressionType =
  case resolveType state expressionType of
    TVarType _ -> True
    _ -> supportsRuntimeEqualityType state expressionType
