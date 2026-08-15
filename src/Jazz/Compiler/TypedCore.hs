{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Semantic typed-core contract shared by the stage-0 compiler and the
-- Jazz-authored compiler.  The representation deliberately keeps malformed
-- states constructible so the boundary validator can report them precisely.
module Jazz.Compiler.TypedCore where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

newtype TypedTypeParameterId = TypedTypeParameterId Int
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype TypedEvidenceParameterId = TypedEvidenceParameterId Int
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype TypedBinderId = TypedBinderId ([Text], [Int], TypedCoreName)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedEvidenceParameterRef = TypedEvidenceParameterRef TypedBinderId TypedEvidenceParameterId
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype TypedSourcePath = TypedSourcePath Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

validTypedSourcePath :: TypedSourcePath -> Bool
validTypedSourcePath (TypedSourcePath sourcePath) =
  not (Text.null sourcePath)
    && not (Text.isPrefixOf "/" sourcePath)
    && not (Text.any (== '\\') sourcePath)
    && not (driveAbsolute sourcePath)
    && all validSegment (Text.splitOn "/" sourcePath)
  where
    validSegment segment = not (Text.null segment) && segment /= "." && segment /= ".."
    driveAbsolute path =
      case Text.unpack path of
        _ : ':' : _ -> True
        _ -> False

data TypedNameOrigin
  = TypedCurrentModule
  | TypedImportedModule [Text]
  | TypedAmbientPrelude
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedNameNamespace
  = TypedValueNamespace
  | TypedConstructorNamespace
  | TypedTypeNamespace
  | TypedCapabilityNamespace
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedGeneratedNameKind
  = TypedLambdaPatternArgument Int
  | TypedOperatorBinding Text
  | TypedOperatorSectionFunction
  | TypedOperatorSectionLeft
  | TypedOperatorSectionRight
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedCoreName
  = TypedUnresolvedSourceName Text
  | TypedUnresolvedQualifiedName Text Text
  | TypedResolvedName TypedNameOrigin TypedNameNamespace Text
  | TypedBuiltinName Text
  | TypedGeneratedName TypedGeneratedNameKind
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedOperatorRef
  = TypedBuiltinOperator Text
  | TypedResolvedOperator TypedCoreName Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedSpan = TypedSpan Int Int
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedNumericType
  = TypedInt8Type
  | TypedInt16Type
  | TypedInt32Type
  | TypedInt64Type
  | TypedUInt8Type
  | TypedUInt16Type
  | TypedUInt32Type
  | TypedUInt64Type
  | TypedFloat16Type
  | TypedFloat32Type
  | TypedFloat64Type
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedType
  = TypedIntType
  | TypedFloatType
  | TypedNumericType TypedNumericType
  | TypedBoolType
  | TypedCharType
  | TypedTextType
  | TypedListType TypedType
  | TypedTupleType [TypedType]
  | TypedDataType TypedCoreName [TypedType]
  | TypedFunctionType TypedType TypedType
  | TypedTypeParameterType TypedTypeParameterId
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedRepresentationRecipe
  = TypedUnitRecipe
  | TypedBoolRecipe
  | TypedSignedIntegerRecipe Int
  | TypedUnsignedIntegerRecipe Int
  | TypedFloatRecipe Int
  | TypedCharRecipe
  | TypedManagedTextRecipe
  | TypedManagedListRecipe TypedRepresentationRecipe
  | TypedManagedProductRecipe [TypedRepresentationRecipe]
  | TypedManagedVariantRecipe TypedCoreName [TypedType]
  | TypedClosureRecipe [TypedRepresentationRecipe] TypedRepresentationRecipe
  | TypedRepresentationParameterRecipe TypedTypeParameterId
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedNumericConstraint
  = TypedAnyNumericConstraint
  | TypedRuntimeArithmeticNumericConstraint
  | TypedRuntimeComparisonNumericConstraint
  | TypedIntegralNumericConstraint
  | TypedIntegralLiteralNumericConstraint Text Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedPrimitiveConstraint
  = TypedNumericPrimitiveConstraint TypedNumericConstraint TypedType
  | TypedStrictEqualityPrimitiveConstraint TypedType
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedCapabilityConstraint = TypedCapabilityConstraint TypedCoreName (Maybe Text) TypedType
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedEvidenceParameter = TypedEvidenceParameter TypedEvidenceParameterId TypedCapabilityConstraint
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedCallableShape
  = TypedDirectCallableShape
  | TypedClosureCallableShape
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedScheme
  = TypedScheme
      TypedBinderId
      [TypedTypeParameterId]
      [TypedEvidenceParameter]
      [TypedPrimitiveConstraint]
      TypedType
      TypedRepresentationRecipe
      (Maybe TypedCallableShape)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedTypeArgument = TypedTypeArgument TypedTypeParameterId TypedType
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedInstantiation = TypedInstantiation TypedBinderId [TypedTypeArgument] (Maybe TypedSpan)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedImplId = TypedImplId [Text] TypedCoreName [TypedType]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedMethodId = TypedMethodId TypedImplId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedEvidenceUse
  = TypedEvidenceUse
      (Maybe TypedEvidenceParameterRef)
      TypedCapabilityConstraint
      TypedImplId
      (Maybe TypedMethodId)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedEvidenceCandidate = TypedEvidenceCandidate TypedImplId (Maybe TypedMethodId)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedEvidenceSelection
  = TypedSelectedEvidence TypedEvidenceUse
  | TypedEvidenceCandidates TypedCapabilityConstraint [TypedEvidenceCandidate]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedNodeInfo
  = TypedNodeInfo
      TypedType
      TypedRepresentationRecipe
      [TypedInstantiation]
      [TypedEvidenceSelection]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedLiteral
  = TypedIntegerLiteral Text
  | TypedFractionalLiteral Text Text (Maybe TypedNumericType)
  | TypedBooleanLiteral Bool
  | TypedCharacterLiteral Char
  | TypedTextLiteral Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedPattern
  = TypedWildcardPattern TypedNodeInfo
  | TypedVariablePattern TypedNodeInfo TypedBinderId TypedCoreName
  | TypedLiteralPattern TypedNodeInfo TypedLiteral
  | TypedConstructorPattern TypedNodeInfo TypedCoreName [TypedPattern]
  | TypedListPattern TypedNodeInfo [TypedPattern]
  | TypedConsListPattern TypedNodeInfo TypedPattern TypedPattern
  | TypedTuplePattern TypedNodeInfo [TypedPattern]
  | TypedAsPattern TypedNodeInfo TypedBinderId TypedCoreName TypedPattern
  | TypedOrPattern TypedNodeInfo [TypedPattern]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedCaseArm = TypedCaseArm TypedPattern (Maybe TypedExpr) TypedExpr
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedExpr
  = TypedLiteralExpr TypedNodeInfo TypedLiteral
  | TypedVariableExpr TypedNodeInfo TypedCoreName (Maybe TypedBinderId)
  | TypedLambdaExpr TypedNodeInfo TypedBinderId TypedCoreName TypedExpr
  | TypedOperatorValueExpr TypedNodeInfo TypedOperatorRef
  | TypedListExpr TypedNodeInfo [TypedExpr]
  | TypedTupleExpr TypedNodeInfo [TypedExpr]
  | TypedApplyExpr TypedNodeInfo TypedExpr TypedExpr
  | TypedTypeApplicationExpr TypedNodeInfo TypedExpr TypedSpan TypedType
  | TypedIfExpr TypedNodeInfo TypedExpr TypedExpr TypedExpr
  | TypedPatternCaseExpr TypedNodeInfo TypedExpr [TypedCaseArm]
  | TypedBinaryExpr TypedNodeInfo TypedOperatorRef TypedExpr TypedExpr
  | TypedLeftSectionExpr TypedNodeInfo TypedExpr TypedOperatorRef
  | TypedRightSectionExpr TypedNodeInfo TypedOperatorRef TypedExpr
  | TypedBlockExpr TypedNodeInfo [TypedStatement]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

typedExpressionInfo :: TypedExpr -> TypedNodeInfo
typedExpressionInfo expression =
  case expression of
    TypedLiteralExpr info _ -> info
    TypedVariableExpr info _ _ -> info
    TypedLambdaExpr info _ _ _ -> info
    TypedOperatorValueExpr info _ -> info
    TypedListExpr info _ -> info
    TypedTupleExpr info _ -> info
    TypedApplyExpr info _ _ -> info
    TypedTypeApplicationExpr info _ _ _ -> info
    TypedIfExpr info _ _ _ -> info
    TypedPatternCaseExpr info _ _ -> info
    TypedBinaryExpr info _ _ _ -> info
    TypedLeftSectionExpr info _ _ -> info
    TypedRightSectionExpr info _ _ -> info
    TypedBlockExpr info _ -> info

typedNodeType :: TypedNodeInfo -> TypedType
typedNodeType (TypedNodeInfo typeValue _ _ _) = typeValue

typedNodeRecipe :: TypedNodeInfo -> TypedRepresentationRecipe
typedNodeRecipe (TypedNodeInfo _ recipe _ _) = recipe

data TypedConstructorDeclaration
  = TypedConstructorDeclaration
      TypedBinderId
      TypedCoreName
      [TypedType]
      [TypedRepresentationRecipe]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedDataDeclaration
  = TypedDataDeclaration
      TypedSpan
      TypedCoreName
      [TypedTypeParameterId]
      [TypedConstructorDeclaration]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedMethodSignature = TypedMethodSignature TypedCoreName TypedSpan TypedScheme
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedClassDeclaration
  = TypedClassDeclaration
      TypedSpan
      TypedCoreName
      [TypedTypeParameterId]
      [TypedMethodSignature]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedMethodDefinition
  = TypedMethodDefinition
      TypedMethodId
      TypedBinderId
      TypedCoreName
      TypedSpan
      TypedExpr
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedImplDeclaration = TypedImplDeclaration TypedSpan TypedImplId [TypedMethodDefinition]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedStatement
  = TypedLetStatement TypedBinderId TypedCoreName TypedSpan TypedScheme TypedExpr
  | TypedSignatureStatement TypedBinderId TypedCoreName TypedSpan TypedScheme
  | TypedDataStatement TypedDataDeclaration
  | TypedClassStatement TypedClassDeclaration
  | TypedImplStatement TypedImplDeclaration
  | TypedExpressionStatement TypedSpan TypedExpr
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedResolvedImport = TypedResolvedImport TypedSpan [Text] (Maybe Text) (Maybe [Text])
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedModuleExport = TypedModuleExport TypedNameNamespace Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedValueInterface = TypedValueInterface TypedCoreName TypedScheme
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype TypedDataInterface = TypedDataInterface TypedDataDeclaration
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype TypedClassInterface = TypedClassInterface TypedClassDeclaration
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype TypedImplInterface = TypedImplInterface TypedImplId
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedModuleInterface
  = TypedModuleInterface
      [TypedValueInterface]
      [TypedDataInterface]
      [TypedClassInterface]
      [TypedImplInterface]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

newtype TypedRecursiveGroup = TypedRecursiveGroup [TypedBinderId]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedModule
  = TypedModule
      [Text]
      TypedSourcePath
      [TypedResolvedImport]
      [TypedModuleExport]
      TypedModuleInterface
      [TypedRecursiveGroup]
      [TypedStatement]
      TypedNodeInfo
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedProgram = TypedProgram (Maybe TypedModule) [TypedModule] [Text]
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data TypedCoreValidationPath
  = TypedProgramPath
  | TypedPreludePath
  | TypedModulePath [Text]
  | TypedInterfacePath [Text]
  | TypedStatementPath [Text] [Int]
  | TypedExpressionPath [Text] [Int] [Int]
  | TypedPatternPath [Text] [Int] [Int]
  deriving (Eq, Ord, Show)

data TypedCoreValidationKind
  = TypedUnresolvedName
  | TypedInvalidSourcePath
  | TypedInvalidSpan
  | TypedDuplicateModule
  | TypedUnknownEntryModule
  | TypedDuplicateBinder
  | TypedDuplicateDeclaration
  | TypedUnknownBinder
  | TypedDuplicateTypeParameter
  | TypedInvalidTypeParameterOrder
  | TypedUnboundTypeParameter
  | TypedUnboundRepresentationParameter
  | TypedInvalidRepresentationWidth
  | TypedTypeRepresentationMismatch
  | TypedApplicationFunctionMismatch
  | TypedApplicationArgumentMismatch
  | TypedApplicationResultMismatch
  | TypedConditionalConditionMismatch
  | TypedConditionalBranchMismatch
  | TypedPatternScrutineeMismatch
  | TypedPatternGuardMismatch
  | TypedPatternArmResultMismatch
  | TypedOrPatternBinderMismatch
  | TypedDuplicateEvidenceParameter
  | TypedInvalidEvidenceParameterOrder
  | TypedInstantiationMismatch
  | TypedMissingEvidence
  | TypedDuplicateEvidence
  | TypedAmbiguousEvidence
  | TypedInvisibleName
  | TypedInvisibleImpl
  | TypedMethodSelectionMismatch
  | TypedBindingValueMismatch
  | TypedLambdaResultMismatch
  | TypedLiteralTypeMismatch
  | TypedCollectionShapeMismatch
  | TypedDataTypeMismatch
  | TypedPatternShapeMismatch
  | TypedBlockResultMismatch
  | TypedModuleResultMismatch
  | TypedDataRecipeMismatch
  | TypedCallableRecipeMismatch
  | TypedCallableShapeMismatch
  | TypedRecursiveGroupMismatch
  | TypedBinderReferenceMismatch
  | TypedModuleInterfaceMismatch
  deriving (Bounded, Enum, Eq, Ord, Show)

data TypedCoreValidationDetail
  = TypedNoValidationDetail
  | TypedTextDetail Text
  | TypedIndexDetail Int
  | TypedArityDetail Int Int
  | TypedNameDetail TypedCoreName
  | TypedBinderDetail TypedBinderId
  | TypedTypeDetail TypedType TypedType
  | TypedRecipeDetail TypedRepresentationRecipe TypedRepresentationRecipe
  | TypedTypeParameterDetail TypedTypeParameterId
  | TypedEvidenceParameterDetail TypedEvidenceParameterId
  | TypedImplDetail TypedImplId
  deriving (Eq, Ord, Show)

data TypedCoreValidationFailure
  = TypedCoreValidationFailure
      TypedCoreValidationPath
      TypedCoreValidationKind
      TypedCoreValidationDetail
  deriving (Eq, Ord, Show)

data TypedCoreOutcome
  = TypedCoreBlockedByDiagnostics
  | TypedCoreInvariantFailures [TypedCoreValidationFailure]
  | TypedCoreSucceeded TypedProgram
  deriving (Eq, Ord, Show)
