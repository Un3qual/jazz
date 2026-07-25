{-# LANGUAGE OverloadedStrings #-}

-- | Semantic typed-core contract shared by the stage-0 compiler and the
-- Jazz-authored compiler.  The representation deliberately keeps malformed
-- states constructible so the boundary validator can report them precisely.
module JazzNext.Compiler.TypedCore where

import Data.Text (Text)

newtype TypedTypeParameterId = TypedTypeParameterId Int
  deriving (Eq, Ord, Show)

newtype TypedEvidenceParameterId = TypedEvidenceParameterId Int
  deriving (Eq, Ord, Show)

newtype TypedBinderId = TypedBinderId ([Text], [Int], TypedCoreName)
  deriving (Eq, Ord, Show)

data TypedEvidenceParameterRef = TypedEvidenceParameterRef TypedBinderId TypedEvidenceParameterId
  deriving (Eq, Ord, Show)

newtype TypedSourcePath = TypedSourcePath Text
  deriving (Eq, Ord, Show)

data TypedNameOrigin
  = TypedCurrentModule
  | TypedImportedModule [Text]
  | TypedAmbientPrelude
  deriving (Eq, Ord, Show)

data TypedNameNamespace
  = TypedValueNamespace
  | TypedConstructorNamespace
  | TypedTypeNamespace
  | TypedCapabilityNamespace
  deriving (Eq, Ord, Show)

data TypedGeneratedNameKind
  = TypedLambdaPatternArgument Int
  | TypedOperatorBinding Text
  | TypedOperatorSectionFunction
  | TypedOperatorSectionLeft
  | TypedOperatorSectionRight
  deriving (Eq, Ord, Show)

data TypedCoreName
  = TypedUnresolvedSourceName Text
  | TypedUnresolvedQualifiedName Text Text
  | TypedResolvedName TypedNameOrigin TypedNameNamespace Text
  | TypedBuiltinName Text
  | TypedGeneratedName TypedGeneratedNameKind
  deriving (Eq, Ord, Show)

data TypedOperatorRef
  = TypedBuiltinOperator Text
  | TypedResolvedOperator TypedCoreName Text
  deriving (Eq, Ord, Show)

data TypedSpan = TypedSpan Int Int
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

data TypedNumericConstraint
  = TypedAnyNumericConstraint
  | TypedRuntimeArithmeticNumericConstraint
  | TypedRuntimeComparisonNumericConstraint
  | TypedIntegralNumericConstraint
  | TypedIntegralLiteralNumericConstraint Text Text
  deriving (Eq, Ord, Show)

data TypedPrimitiveConstraint
  = TypedNumericPrimitiveConstraint TypedNumericConstraint TypedType
  | TypedStrictEqualityPrimitiveConstraint TypedType
  deriving (Eq, Ord, Show)

data TypedCapabilityConstraint = TypedCapabilityConstraint Text (Maybe Text) TypedType
  deriving (Eq, Ord, Show)

data TypedEvidenceParameter = TypedEvidenceParameter TypedEvidenceParameterId TypedCapabilityConstraint
  deriving (Eq, Ord, Show)

data TypedScheme = TypedScheme
  TypedBinderId
  [TypedTypeParameterId]
  [TypedEvidenceParameter]
  [TypedPrimitiveConstraint]
  TypedType
  TypedRepresentationRecipe
  deriving (Eq, Ord, Show)

data TypedTypeArgument = TypedTypeArgument TypedTypeParameterId TypedType
  deriving (Eq, Ord, Show)

data TypedInstantiation = TypedInstantiation TypedBinderId [TypedTypeArgument] (Maybe TypedSpan)
  deriving (Eq, Ord, Show)

data TypedImplId = TypedImplId [Text] TypedCoreName [TypedType]
  deriving (Eq, Ord, Show)

data TypedMethodId = TypedMethodId TypedImplId Text
  deriving (Eq, Ord, Show)

data TypedEvidenceUse = TypedEvidenceUse
  (Maybe TypedEvidenceParameterRef)
  TypedCapabilityConstraint
  TypedImplId
  (Maybe TypedMethodId)
  deriving (Eq, Ord, Show)

data TypedEvidenceCandidate = TypedEvidenceCandidate TypedImplId (Maybe TypedMethodId)
  deriving (Eq, Ord, Show)

data TypedEvidenceSelection
  = TypedSelectedEvidence TypedEvidenceUse
  | TypedEvidenceCandidates TypedCapabilityConstraint [TypedEvidenceCandidate]
  deriving (Eq, Ord, Show)

data TypedNodeInfo = TypedNodeInfo
  TypedType
  TypedRepresentationRecipe
  [TypedInstantiation]
  [TypedEvidenceSelection]
  deriving (Eq, Ord, Show)

data TypedLiteral
  = TypedIntegerLiteral Text
  | TypedFractionalLiteral Text Text (Maybe TypedNumericType)
  | TypedBooleanLiteral Bool
  | TypedCharacterLiteral Char
  | TypedTextLiteral Text
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

data TypedCaseArm = TypedCaseArm TypedPattern (Maybe TypedExpr) TypedExpr
  deriving (Eq, Ord, Show)

data TypedExpr
  = TypedLiteralExpr TypedNodeInfo TypedLiteral
  | TypedVariableExpr TypedNodeInfo TypedCoreName
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
  deriving (Eq, Ord, Show)

data TypedConstructorDeclaration = TypedConstructorDeclaration
  TypedBinderId
  TypedCoreName
  [TypedType]
  [TypedRepresentationRecipe]
  deriving (Eq, Ord, Show)

data TypedDataDeclaration = TypedDataDeclaration
  TypedSpan
  TypedCoreName
  [TypedTypeParameterId]
  [TypedConstructorDeclaration]
  deriving (Eq, Ord, Show)

data TypedMethodSignature = TypedMethodSignature TypedCoreName TypedSpan TypedScheme
  deriving (Eq, Ord, Show)

data TypedClassDeclaration = TypedClassDeclaration
  TypedSpan
  TypedCoreName
  [TypedTypeParameterId]
  [TypedMethodSignature]
  deriving (Eq, Ord, Show)

data TypedMethodDefinition = TypedMethodDefinition
  TypedMethodId
  TypedBinderId
  TypedCoreName
  TypedSpan
  TypedExpr
  deriving (Eq, Ord, Show)

data TypedImplDeclaration = TypedImplDeclaration TypedSpan TypedImplId [TypedMethodDefinition]
  deriving (Eq, Ord, Show)

data TypedStatement
  = TypedLetStatement TypedBinderId TypedCoreName TypedSpan TypedScheme TypedExpr
  | TypedSignatureStatement TypedBinderId TypedCoreName TypedSpan TypedScheme
  | TypedDataStatement TypedDataDeclaration
  | TypedClassStatement TypedClassDeclaration
  | TypedImplStatement TypedImplDeclaration
  | TypedExpressionStatement TypedSpan TypedExpr
  deriving (Eq, Ord, Show)

data TypedResolvedImport = TypedResolvedImport TypedSpan [Text] (Maybe Text) (Maybe [Text])
  deriving (Eq, Ord, Show)

data TypedModuleExport = TypedModuleExport TypedNameNamespace Text
  deriving (Eq, Ord, Show)

data TypedValueInterface = TypedValueInterface TypedCoreName TypedScheme
  deriving (Eq, Ord, Show)

newtype TypedDataInterface = TypedDataInterface TypedDataDeclaration
  deriving (Eq, Ord, Show)

newtype TypedClassInterface = TypedClassInterface TypedClassDeclaration
  deriving (Eq, Ord, Show)

newtype TypedImplInterface = TypedImplInterface TypedImplId
  deriving (Eq, Ord, Show)

data TypedModuleInterface = TypedModuleInterface
  [TypedValueInterface]
  [TypedDataInterface]
  [TypedClassInterface]
  [TypedImplInterface]
  deriving (Eq, Ord, Show)

data TypedModule = TypedModule
  [Text]
  TypedSourcePath
  [TypedResolvedImport]
  [TypedModuleExport]
  TypedModuleInterface
  [TypedStatement]
  TypedNodeInfo
  deriving (Eq, Ord, Show)

data TypedProgram = TypedProgram (Maybe TypedModule) [TypedModule] [Text]
  deriving (Eq, Ord, Show)

data TypedCoreValidationPath
  = TypedProgramPath
  | TypedPreludePath
  | TypedModulePath [Text]
  | TypedInterfacePath [Text]
  | TypedStatementPath [Text] Int
  | TypedExpressionPath [Text] Int [Int]
  | TypedPatternPath [Text] Int [Int]
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

data TypedCoreValidationFailure = TypedCoreValidationFailure
  TypedCoreValidationPath
  TypedCoreValidationKind
  TypedCoreValidationDetail
  deriving (Eq, Ord, Show)

data TypedCoreOutcome
  = TypedCoreBlockedByDiagnostics
  | TypedCoreInvariantFailures [TypedCoreValidationFailure]
  | TypedCoreSucceeded TypedProgram
  deriving (Eq, Ord, Show)
