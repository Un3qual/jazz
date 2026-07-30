{-# LANGUAGE OverloadedStrings #-}

-- | Opt-in, deliberately narrow typed-core production support.  The ordinary
-- inference path does not retain these values; they are used only by the
-- explicit resolved-module producer.
module JazzNext.Compiler.TypeInference.Elaboration
  ( TypedCoreProductionProfile (..),
    TypedCoreProductionStatus (..),
    TypedCoreProductionFailure (..),
    TypedCoreProductionPath (..),
    TypedCoreProductionFailureKind (..),
    TypedCoreProductionFailureDetail (..),
    TypedCoreProductionMode (..),
    InferredExpr (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedScope (..),
    finalizeTypedCoreExpressionDirectCall,
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST (Literal (..), NumericType (..))
import JazzNext.Compiler.Diagnostics (SourceSpan (..))
import JazzNext.Compiler.FractionalLiteral (fractionalLiteralSourceParts)
import JazzNext.Compiler.ModuleGraph (ResolvedModule (..))
import JazzNext.Compiler.TypedCore
import JazzNext.Compiler.TypedCore.Validate (validateTypedProgram)
import JazzNext.Compiler.TypeInference.Solver (resolveType)
import JazzNext.Compiler.TypeInference.State (InferState)
import JazzNext.Compiler.TypeInference.Types (ExpressionType (..))

data TypedCoreProductionProfile
  = TypedCoreExpressionDirectCallProfile
  deriving (Eq, Show)

data TypedCoreProductionStatus
  = TypedCoreProductionBlockedByDiagnostics
  | TypedCoreProductionUnsupported [TypedCoreProductionFailure]
  | TypedCoreProductionInvariantFailures [TypedCoreValidationFailure]
  | TypedCoreProductionSucceeded TypedProgram
  deriving (Eq, Show)

data TypedCoreProductionPath
  = TypedCoreProductionInputPath
  | TypedCoreProductionModulePath [Text]
  | TypedCoreProductionStatementPath [Text] Int
  | TypedCoreProductionExpressionPath [Text] Int [Int]
  deriving (Eq, Show)

data TypedCoreProductionFailureKind
  = TypedCoreModulePathMismatch
  | TypedCoreInvalidPortableSourcePath
  | TypedCoreResolvedImportsUnsupported
  | TypedCoreImportedInputsUnsupported
  | TypedCoreAmbientPreludeInputUnsupported
  | TypedCoreUnsupportedRootExpression
  | TypedCoreManagedValueUnsupported
  | TypedCoreStructuredValueUnsupported
  | TypedCoreControlFlowUnsupported
  | TypedCorePatternCaseUnsupported
  | TypedCoreNestedBlockUnsupported
  | TypedCoreUserDefinedOperatorUnsupported
  | TypedCoreUnresolvedExpressionType
  deriving (Eq, Show)

data TypedCoreProductionFailureDetail
  = TypedCoreNoFailureDetail
  | TypedCoreTextValueDetail
  | TypedCoreListValueDetail
  | TypedCoreTupleValueDetail
  | TypedCoreDataValueDetail
  | TypedCoreConditionalDetail
  | TypedCorePatternCaseDetail
  | TypedCoreLocalBlockDetail
  | TypedCoreUnsupportedRootDetail
  deriving (Eq, Show)

data TypedCoreProductionFailure
  = TypedCoreProductionFailure
      TypedCoreProductionPath
      TypedCoreProductionFailureKind
      TypedCoreProductionFailureDetail
  deriving (Eq, Show)

data TypedCoreProductionMode
  = InferenceOnly
  | ProduceTypedCoreExpressionDirectCall
  deriving (Eq, Show)

-- | The private result threaded by production-aware inference.  Existing
-- inference-only helpers construct this with no retained node or failures.
data InferredExpr = InferredExpr
  { inferredExpressionType :: Maybe ExpressionType,
    inferredProvisionalExpr :: Maybe ProvisionalTypedExpr,
    inferredProductionFailures :: [TypedCoreProductionFailure]
  }
  deriving (Eq, Show)

data ProvisionalTypedExpr
  = ProvisionalUnitExpression
  | ProvisionalLiteralExpression Literal ExpressionType
  | ProvisionalBinaryExpression Text ExpressionType ProvisionalTypedExpr ProvisionalTypedExpr
  | ProvisionalScopeExpressions [(SourceSpan, ProvisionalTypedExpr)]
  | ProvisionalUnsupportedExpression TypedCoreProductionFailureKind TypedCoreProductionFailureDetail
  deriving (Eq, Show)

newtype ProvisionalTypedScope = ProvisionalTypedScope ProvisionalTypedExpr
  deriving (Eq, Show)

-- | Finalize the initial unit-only root against the permanent contract.
-- Future profile slices extend the provisional scope rather than changing the
-- typed-core constructors themselves.
finalizeTypedCoreExpressionDirectCall ::
  TypedSourcePath ->
  ResolvedModule ->
  InferState ->
  ProvisionalTypedScope ->
  TypedCoreProductionStatus
finalizeTypedCoreExpressionDirectCall sourcePath resolvedModule state (ProvisionalTypedScope provisionalScope) =
  case provisionalScope of
    ProvisionalScopeExpressions scopedExpressions ->
      case traverse (uncurry finalizeStatement) (zip [0 ..] scopedExpressions) of
        Left failure -> TypedCoreProductionUnsupported [failure]
        Right typedStatements ->
          case validateTypedProgram (typedProgram typedStatements) of
            [] -> TypedCoreProductionSucceeded (typedProgram typedStatements)
            failures -> TypedCoreProductionInvariantFailures failures
    ProvisionalUnsupportedExpression kind detail ->
      TypedCoreProductionUnsupported [failureAt 0 [] kind detail]
    _ -> TypedCoreProductionUnsupported [failureAt 0 [] TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
  where
    modulePath = resolvedModulePath resolvedModule

    typedProgram typedStatements =
      TypedProgram Nothing
        [TypedModule modulePath sourcePath [] [] (TypedModuleInterface [] [] [] []) typedStatements (typedStatementInfo (last typedStatements))]
        modulePath

    failureAt statementIndex childPath kind detail =
      TypedCoreProductionFailure (TypedCoreProductionExpressionPath modulePath statementIndex childPath) kind detail

    finalizeStatement statementIndex (spanValue, expression) = do
      typedExpression <- finalizeExpression statementIndex [] expression
      Right (TypedExpressionStatement (typedSpan spanValue) typedExpression)

    finalizeExpression statementIndex childPath expression =
      case expression of
        ProvisionalUnitExpression ->
          Right (TypedTupleExpr unitInfo [])
        ProvisionalLiteralExpression literal expressionType -> do
          info <- scalarInfo statementIndex childPath expressionType
          literalValue <- typedLiteral statementIndex childPath literal info
          Right (TypedLiteralExpr info literalValue)
        ProvisionalBinaryExpression operatorSymbol expressionType left right
          | operatorSymbol `elem` admittedOperators -> do
              info <- scalarInfo statementIndex childPath expressionType
              leftExpression <- finalizeExpression statementIndex (childPath <> [0]) left
              rightExpression <- finalizeExpression statementIndex (childPath <> [1]) right
              Right (TypedBinaryExpr info (TypedBuiltinOperator operatorSymbol) leftExpression rightExpression)
          | otherwise -> Left (failureAt statementIndex childPath TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail)
        ProvisionalScopeExpressions _ -> Left (failureAt statementIndex childPath TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail)
        ProvisionalUnsupportedExpression kind detail -> Left (failureAt statementIndex childPath kind detail)

    scalarInfo statementIndex childPath expressionType =
      case defaultScalarLiterals (resolveType state expressionType) of
        TIntType -> Right (TypedNodeInfo TypedIntType (TypedSignedIntegerRecipe 64) [] [])
        TIntegerLiteralType {} -> Right (TypedNodeInfo TypedIntType (TypedSignedIntegerRecipe 64) [] [])
        TFloatType -> Right (TypedNodeInfo TypedFloatType (TypedFloatRecipe 64) [] [])
        TNumericType numericType ->
          let (numericTypeValue, recipe) = numericInfo numericType
           in Right (TypedNodeInfo (TypedNumericType numericTypeValue) recipe [] [])
        TBoolType -> Right (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [])
        TCharType -> Right (TypedNodeInfo TypedCharType TypedCharRecipe [] [])
        TTextType -> Left (failureAt statementIndex childPath TypedCoreManagedValueUnsupported TypedCoreTextValueDetail)
        TListType {} -> Left (failureAt statementIndex childPath TypedCoreStructuredValueUnsupported TypedCoreListValueDetail)
        TTupleType [] -> Right unitInfo
        TTupleType {} -> Left (failureAt statementIndex childPath TypedCoreStructuredValueUnsupported TypedCoreTupleValueDetail)
        TDataType {} -> Left (failureAt statementIndex childPath TypedCoreStructuredValueUnsupported TypedCoreDataValueDetail)
        TFunctionType {} -> Left (failureAt statementIndex childPath TypedCoreManagedValueUnsupported TypedCoreUnsupportedRootDetail)
        TVarType {} -> Left (failureAt statementIndex childPath TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail)

    typedLiteral statementIndex childPath literal info =
      case (literal, nodeType info) of
        (LInt value, TypedIntType) -> Right (TypedIntegerLiteral (Text.pack (show value)))
        (LInt value, TypedNumericType _) -> Right (TypedIntegerLiteral (Text.pack (show value)))
        (LFloat _ source _, TypedFloatType) -> Right (fractionalLiteral source Nothing)
        (LFloat _ source (Just numericType), TypedNumericType _) -> Right (fractionalLiteral source (Just (typedNumericType numericType)))
        (LBool value, TypedBoolType) -> Right (TypedBooleanLiteral value)
        (LChar value, TypedCharType) -> Right (TypedCharacterLiteral value)
        (LText _, _) -> Left (failureAt statementIndex childPath TypedCoreManagedValueUnsupported TypedCoreTextValueDetail)
        _ -> Left (failureAt statementIndex childPath TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail)

    fractionalLiteral source maybeNumericType =
      let (whole, fractional, _) = fractionalLiteralSourceParts source
       in TypedFractionalLiteral (Text.pack (show whole)) (Text.pack (show (abs fractional))) maybeNumericType

    unitInfo = TypedNodeInfo (TypedTupleType []) TypedUnitRecipe [] []

admittedOperators :: [Text]
admittedOperators = ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!="]

typedSpan :: SourceSpan -> TypedSpan
typedSpan spanValue = TypedSpan (spanLine spanValue) (spanColumn spanValue)

numericInfo :: NumericType -> (TypedNumericType, TypedRepresentationRecipe)
numericInfo numericType =
  case numericType of
    NumericInt8 -> (TypedInt8Type, TypedSignedIntegerRecipe 8)
    NumericInt16 -> (TypedInt16Type, TypedSignedIntegerRecipe 16)
    NumericInt32 -> (TypedInt32Type, TypedSignedIntegerRecipe 32)
    NumericInt64 -> (TypedInt64Type, TypedSignedIntegerRecipe 64)
    NumericUInt8 -> (TypedUInt8Type, TypedUnsignedIntegerRecipe 8)
    NumericUInt16 -> (TypedUInt16Type, TypedUnsignedIntegerRecipe 16)
    NumericUInt32 -> (TypedUInt32Type, TypedUnsignedIntegerRecipe 32)
    NumericUInt64 -> (TypedUInt64Type, TypedUnsignedIntegerRecipe 64)
    NumericFloat16 -> (TypedFloat16Type, TypedFloatRecipe 16)
    NumericFloat32 -> (TypedFloat32Type, TypedFloatRecipe 32)
    NumericFloat64 -> (TypedFloat64Type, TypedFloatRecipe 64)

typedNumericType :: NumericType -> TypedNumericType
typedNumericType = fst . numericInfo

typedExpressionInfo :: TypedExpr -> TypedNodeInfo
typedExpressionInfo expression =
  case expression of
    TypedLiteralExpr info _ -> info
    TypedTupleExpr info _ -> info
    TypedBinaryExpr info _ _ _ -> info
    _ -> error "scalar typed-core elaboration produced a non-scalar expression"

typedStatementInfo :: TypedStatement -> TypedNodeInfo
typedStatementInfo statement =
  case statement of
    TypedExpressionStatement _ expression -> typedExpressionInfo expression
    _ -> error "scalar typed-core elaboration produced a non-expression statement"

nodeType :: TypedNodeInfo -> TypedType
nodeType (TypedNodeInfo typeValue _ _ _) = typeValue

defaultScalarLiterals :: ExpressionType -> ExpressionType
defaultScalarLiterals expressionType =
  case expressionType of
    TIntegerLiteralType {} -> TIntType
    _ -> expressionType
