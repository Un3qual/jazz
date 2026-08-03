{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.CanonicalTypedCoreComparison
  ( canonicalTypedProgramRuntimeValue,
    canonicalTypedCoreOutcomeRuntimeValue,
    canonicalTypedValidationFailuresRuntimeValue,
    CanonicalTypedCoreStructure,
    decodeCanonicalTypedCoreStructure,
    decodeCanonicalTypedValidationFailuresRuntimeValue,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.CanonicalValue
  ( canonicalConstructor,
    canonicalNullaryConstructor,
    runtimeIntValue,
  )
import Jazz.Compiler.Name (identifierText)
import Jazz.Compiler.Runtime (RuntimeValue (..))
import Jazz.Compiler.TypedCore

data CanonicalTypedCoreStructure
  = CanonicalStructureInt Integer
  | CanonicalStructureBool Bool
  | CanonicalStructureChar Char
  | CanonicalStructureText Text
  | CanonicalStructureList [CanonicalTypedCoreStructure]
  | CanonicalStructureTuple [CanonicalTypedCoreStructure]
  | CanonicalStructureConstructor Text [CanonicalTypedCoreStructure]
  deriving (Eq, Show)

decodeCanonicalTypedCoreStructure :: RuntimeValue -> Either Text CanonicalTypedCoreStructure
decodeCanonicalTypedCoreStructure value =
  case value of
    VInt integer _ -> Right (CanonicalStructureInt integer)
    VBool boolean -> Right (CanonicalStructureBool boolean)
    VChar character -> Right (CanonicalStructureChar character)
    VText textValue -> Right (CanonicalStructureText textValue)
    VList elements _ ->
      CanonicalStructureList <$> traverse decodeCanonicalTypedCoreStructure elements
    VTuple elements ->
      CanonicalStructureTuple <$> traverse decodeCanonicalTypedCoreStructure elements
    VConstructor _ _ constructorName constructorArguments arguments
      | length constructorArguments == length arguments -> do
          canonicalName <-
            canonicalTypedCoreConstructorName (identifierText constructorName)
          CanonicalStructureConstructor canonicalName
            <$> traverse decodeCanonicalTypedCoreStructure arguments
      | otherwise ->
          Left
            ( "canonical typed-core constructor '"
                <> identifierText constructorName
                <> "' is only partially applied"
            )
    VTyped _ innerValue ->
      decodeCanonicalTypedCoreStructure innerValue
    VExplicitTypeApplication _ innerValue ->
      decodeCanonicalTypedCoreStructure innerValue
    _ ->
      Left
        ( "canonical typed-core structure contains unsupported "
            <> runtimeValueCategory value
        )

canonicalTypedCoreConstructorName :: Text -> Either Text Text
canonicalTypedCoreConstructorName name =
  case Text.splitOn "::" name of
    [unqualified] -> Right unqualified
    ["TypedCoreTypes", member] -> Right member
    ["Maybe", member]
      | member == "Just" || member == "Nothing" -> Right member
    _ ->
      Left
        ( "canonical typed-core structure contains constructor outside the checked modules: "
            <> name
        )

canonicalTypedProgramRuntimeValue :: TypedProgram -> RuntimeValue
canonicalTypedProgramRuntimeValue (TypedProgram prelude modules entryModule) =
  constructor
    "TypedProgram"
    [ maybeValue moduleValue prelude,
      listValue moduleValue modules,
      textListValue entryModule
    ]

canonicalTypedCoreOutcomeRuntimeValue :: TypedCoreOutcome -> RuntimeValue
canonicalTypedCoreOutcomeRuntimeValue outcome =
  case outcome of
    TypedCoreBlockedByDiagnostics -> nullary "TypedCoreBlockedByDiagnostics"
    TypedCoreInvariantFailures failures ->
      constructor "TypedCoreInvariantFailures" [canonicalTypedValidationFailuresRuntimeValue failures]
    TypedCoreSucceeded program ->
      constructor "TypedCoreSucceeded" [canonicalTypedProgramRuntimeValue program]

canonicalTypedValidationFailuresRuntimeValue :: [TypedCoreValidationFailure] -> RuntimeValue
canonicalTypedValidationFailuresRuntimeValue = listValue validationFailureValue

decodeCanonicalTypedValidationFailuresRuntimeValue :: RuntimeValue -> Either Text [TypedCoreValidationFailure]
decodeCanonicalTypedValidationFailuresRuntimeValue value =
  decodeList "typed-core validation failures" decodeValidationFailure value

moduleValue :: TypedModule -> RuntimeValue
moduleValue (TypedModule path sourcePath imports exports interface statements info) =
  constructor
    "TypedModule"
    [ textListValue path,
      sourcePathValue sourcePath,
      listValue resolvedImportValue imports,
      listValue moduleExportValue exports,
      moduleInterfaceValue interface,
      listValue statementValue statements,
      nodeInfoValue info
    ]

sourcePathValue :: TypedSourcePath -> RuntimeValue
sourcePathValue (TypedSourcePath path) = constructor "TypedSourcePath" [VText path]

resolvedImportValue :: TypedResolvedImport -> RuntimeValue
resolvedImportValue (TypedResolvedImport spanValue modulePath alias names) =
  constructor
    "TypedResolvedImport"
    [ spanValueOf spanValue,
      textListValue modulePath,
      maybeValue VText alias,
      maybeValue textListValue names
    ]

moduleExportValue :: TypedModuleExport -> RuntimeValue
moduleExportValue (TypedModuleExport namespace name) =
  constructor "TypedModuleExport" [nameNamespaceValue namespace, VText name]

moduleInterfaceValue :: TypedModuleInterface -> RuntimeValue
moduleInterfaceValue (TypedModuleInterface values datas classes impls) =
  constructor
    "TypedModuleInterface"
    [ listValue valueInterfaceValue values,
      listValue dataInterfaceValue datas,
      listValue classInterfaceValue classes,
      listValue implInterfaceValue impls
    ]

valueInterfaceValue :: TypedValueInterface -> RuntimeValue
valueInterfaceValue (TypedValueInterface name scheme) =
  constructor "TypedValueInterface" [coreNameValue name, schemeValue scheme]

dataInterfaceValue :: TypedDataInterface -> RuntimeValue
dataInterfaceValue (TypedDataInterface declaration) =
  constructor "TypedDataInterface" [dataDeclarationValue declaration]

classInterfaceValue :: TypedClassInterface -> RuntimeValue
classInterfaceValue (TypedClassInterface declaration) =
  constructor "TypedClassInterface" [classDeclarationValue declaration]

implInterfaceValue :: TypedImplInterface -> RuntimeValue
implInterfaceValue (TypedImplInterface implId) =
  constructor "TypedImplInterface" [implIdValue implId]

statementValue :: TypedStatement -> RuntimeValue
statementValue statement =
  case statement of
    TypedLetStatement binder name spanValue scheme expression ->
      constructor
        "TypedLetStatement"
        [ binderIdValue binder,
          coreNameValue name,
          spanValueOf spanValue,
          schemeValue scheme,
          expressionValue expression
        ]
    TypedSignatureStatement binder name spanValue scheme ->
      constructor
        "TypedSignatureStatement"
        [binderIdValue binder, coreNameValue name, spanValueOf spanValue, schemeValue scheme]
    TypedDataStatement declaration ->
      constructor "TypedDataStatement" [dataDeclarationValue declaration]
    TypedClassStatement declaration ->
      constructor "TypedClassStatement" [classDeclarationValue declaration]
    TypedImplStatement declaration ->
      constructor "TypedImplStatement" [implDeclarationValue declaration]
    TypedExpressionStatement spanValue expression ->
      constructor "TypedExpressionStatement" [spanValueOf spanValue, expressionValue expression]

dataDeclarationValue :: TypedDataDeclaration -> RuntimeValue
dataDeclarationValue (TypedDataDeclaration spanValue name parameters constructors) =
  constructor
    "TypedDataDeclaration"
    [ spanValueOf spanValue,
      coreNameValue name,
      listValue typeParameterIdValue parameters,
      listValue constructorDeclarationValue constructors
    ]

constructorDeclarationValue :: TypedConstructorDeclaration -> RuntimeValue
constructorDeclarationValue (TypedConstructorDeclaration binder name fields recipes) =
  constructor
    "TypedConstructorDeclaration"
    [ binderIdValue binder,
      coreNameValue name,
      listValue typeValue fields,
      listValue recipeValue recipes
    ]

classDeclarationValue :: TypedClassDeclaration -> RuntimeValue
classDeclarationValue (TypedClassDeclaration spanValue name parameters methods) =
  constructor
    "TypedClassDeclaration"
    [ spanValueOf spanValue,
      coreNameValue name,
      listValue typeParameterIdValue parameters,
      listValue methodSignatureValue methods
    ]

methodSignatureValue :: TypedMethodSignature -> RuntimeValue
methodSignatureValue (TypedMethodSignature name spanValue scheme) =
  constructor "TypedMethodSignature" [coreNameValue name, spanValueOf spanValue, schemeValue scheme]

implDeclarationValue :: TypedImplDeclaration -> RuntimeValue
implDeclarationValue (TypedImplDeclaration spanValue implId methods) =
  constructor
    "TypedImplDeclaration"
    [spanValueOf spanValue, implIdValue implId, listValue methodDefinitionValue methods]

methodDefinitionValue :: TypedMethodDefinition -> RuntimeValue
methodDefinitionValue (TypedMethodDefinition methodId binder name spanValue expression) =
  constructor
    "TypedMethodDefinition"
    [ methodIdValue methodId,
      binderIdValue binder,
      coreNameValue name,
      spanValueOf spanValue,
      expressionValue expression
    ]

expressionValue :: TypedExpr -> RuntimeValue
expressionValue expression =
  case expression of
    TypedLiteralExpr info literal ->
      constructor "TypedLiteralExpr" [nodeInfoValue info, literalValue literal]
    TypedVariableExpr info name ->
      constructor "TypedVariableExpr" [nodeInfoValue info, coreNameValue name]
    TypedLambdaExpr info binder name body ->
      constructor
        "TypedLambdaExpr"
        [nodeInfoValue info, binderIdValue binder, coreNameValue name, expressionValue body]
    TypedOperatorValueExpr info operator ->
      constructor "TypedOperatorValueExpr" [nodeInfoValue info, operatorRefValue operator]
    TypedListExpr info expressions ->
      constructor "TypedListExpr" [nodeInfoValue info, listValue expressionValue expressions]
    TypedTupleExpr info expressions ->
      constructor "TypedTupleExpr" [nodeInfoValue info, listValue expressionValue expressions]
    TypedApplyExpr info function argument ->
      constructor
        "TypedApplyExpr"
        [nodeInfoValue info, expressionValue function, expressionValue argument]
    TypedTypeApplicationExpr info function spanValue typeArgument ->
      constructor
        "TypedTypeApplicationExpr"
        [nodeInfoValue info, expressionValue function, spanValueOf spanValue, typeValue typeArgument]
    TypedIfExpr info condition thenExpression elseExpression ->
      constructor
        "TypedIfExpr"
        [ nodeInfoValue info,
          expressionValue condition,
          expressionValue thenExpression,
          expressionValue elseExpression
        ]
    TypedPatternCaseExpr info scrutinee arms ->
      constructor
        "TypedPatternCaseExpr"
        [nodeInfoValue info, expressionValue scrutinee, listValue caseArmValue arms]
    TypedBinaryExpr info operator left right ->
      constructor
        "TypedBinaryExpr"
        [nodeInfoValue info, operatorRefValue operator, expressionValue left, expressionValue right]
    TypedLeftSectionExpr info left operator ->
      constructor
        "TypedLeftSectionExpr"
        [nodeInfoValue info, expressionValue left, operatorRefValue operator]
    TypedRightSectionExpr info operator right ->
      constructor
        "TypedRightSectionExpr"
        [nodeInfoValue info, operatorRefValue operator, expressionValue right]
    TypedBlockExpr info statements ->
      constructor "TypedBlockExpr" [nodeInfoValue info, listValue statementValue statements]

caseArmValue :: TypedCaseArm -> RuntimeValue
caseArmValue (TypedCaseArm patternValue guard result) =
  constructor
    "TypedCaseArm"
    [ patternValueOf patternValue,
      maybeValue expressionValue guard,
      expressionValue result
    ]

patternValueOf :: TypedPattern -> RuntimeValue
patternValueOf patternValue =
  case patternValue of
    TypedWildcardPattern info ->
      constructor "TypedWildcardPattern" [nodeInfoValue info]
    TypedVariablePattern info binder name ->
      constructor "TypedVariablePattern" [nodeInfoValue info, binderIdValue binder, coreNameValue name]
    TypedLiteralPattern info literal ->
      constructor "TypedLiteralPattern" [nodeInfoValue info, literalValue literal]
    TypedConstructorPattern info name patterns ->
      constructor
        "TypedConstructorPattern"
        [nodeInfoValue info, coreNameValue name, listValue patternValueOf patterns]
    TypedListPattern info patterns ->
      constructor "TypedListPattern" [nodeInfoValue info, listValue patternValueOf patterns]
    TypedConsListPattern info headPattern tailPattern ->
      constructor
        "TypedConsListPattern"
        [nodeInfoValue info, patternValueOf headPattern, patternValueOf tailPattern]
    TypedTuplePattern info patterns ->
      constructor "TypedTuplePattern" [nodeInfoValue info, listValue patternValueOf patterns]
    TypedAsPattern info binder name nested ->
      constructor
        "TypedAsPattern"
        [nodeInfoValue info, binderIdValue binder, coreNameValue name, patternValueOf nested]
    TypedOrPattern info alternatives ->
      constructor "TypedOrPattern" [nodeInfoValue info, listValue patternValueOf alternatives]

nodeInfoValue :: TypedNodeInfo -> RuntimeValue
nodeInfoValue (TypedNodeInfo typeValue' recipe instantiations evidence) =
  constructor
    "TypedNodeInfo"
    [ typeValue typeValue',
      recipeValue recipe,
      listValue instantiationValue instantiations,
      listValue evidenceSelectionValue evidence
    ]

literalValue :: TypedLiteral -> RuntimeValue
literalValue literal =
  case literal of
    TypedIntegerLiteral digits -> constructor "TypedIntegerLiteral" [VText digits]
    TypedFractionalLiteral whole fraction numericType ->
      constructor
        "TypedFractionalLiteral"
        [VText whole, VText fraction, maybeValue numericTypeValue numericType]
    TypedBooleanLiteral value -> constructor "TypedBooleanLiteral" [VBool value]
    TypedCharacterLiteral value -> constructor "TypedCharacterLiteral" [VChar value]
    TypedTextLiteral value -> constructor "TypedTextLiteral" [VText value]

schemeValue :: TypedScheme -> RuntimeValue
schemeValue (TypedScheme binder typeParameters evidence primitiveConstraints typeValue' recipe) =
  constructor
    "TypedScheme"
    [ binderIdValue binder,
      listValue typeParameterIdValue typeParameters,
      listValue evidenceParameterValue evidence,
      listValue primitiveConstraintValue primitiveConstraints,
      typeValue typeValue',
      recipeValue recipe
    ]

primitiveConstraintValue :: TypedPrimitiveConstraint -> RuntimeValue
primitiveConstraintValue constraint =
  case constraint of
    TypedNumericPrimitiveConstraint numericConstraint typeValue' ->
      constructor
        "TypedNumericPrimitiveConstraint"
        [numericConstraintValue numericConstraint, typeValue typeValue']
    TypedStrictEqualityPrimitiveConstraint typeValue' ->
      constructor "TypedStrictEqualityPrimitiveConstraint" [typeValue typeValue']

numericConstraintValue :: TypedNumericConstraint -> RuntimeValue
numericConstraintValue constraint =
  case constraint of
    TypedAnyNumericConstraint -> nullary "TypedAnyNumericConstraint"
    TypedRuntimeArithmeticNumericConstraint -> nullary "TypedRuntimeArithmeticNumericConstraint"
    TypedRuntimeComparisonNumericConstraint -> nullary "TypedRuntimeComparisonNumericConstraint"
    TypedIntegralNumericConstraint -> nullary "TypedIntegralNumericConstraint"
    TypedIntegralLiteralNumericConstraint lower upper ->
      constructor "TypedIntegralLiteralNumericConstraint" [VText lower, VText upper]

evidenceParameterValue :: TypedEvidenceParameter -> RuntimeValue
evidenceParameterValue (TypedEvidenceParameter parameterId constraint) =
  constructor
    "TypedEvidenceParameter"
    [evidenceParameterIdValue parameterId, capabilityConstraintValue constraint]

capabilityConstraintValue :: TypedCapabilityConstraint -> RuntimeValue
capabilityConstraintValue (TypedCapabilityConstraint capability method typeValue') =
  constructor
    "TypedCapabilityConstraint"
    [coreNameValue capability, maybeValue VText method, typeValue typeValue']

instantiationValue :: TypedInstantiation -> RuntimeValue
instantiationValue (TypedInstantiation binder arguments explicitSpan) =
  constructor
    "TypedInstantiation"
    [ binderIdValue binder,
      listValue typeArgumentValue arguments,
      maybeValue spanValueOf explicitSpan
    ]

typeArgumentValue :: TypedTypeArgument -> RuntimeValue
typeArgumentValue (TypedTypeArgument parameterId typeValue') =
  constructor "TypedTypeArgument" [typeParameterIdValue parameterId, typeValue typeValue']

evidenceSelectionValue :: TypedEvidenceSelection -> RuntimeValue
evidenceSelectionValue selection =
  case selection of
    TypedSelectedEvidence evidenceUse ->
      constructor "TypedSelectedEvidence" [evidenceUseValue evidenceUse]
    TypedEvidenceCandidates constraint candidates ->
      constructor
        "TypedEvidenceCandidates"
        [capabilityConstraintValue constraint, listValue evidenceCandidateValue candidates]

evidenceUseValue :: TypedEvidenceUse -> RuntimeValue
evidenceUseValue (TypedEvidenceUse parameterId constraint implId methodId) =
  constructor
    "TypedEvidenceUse"
    [ maybeValue evidenceParameterRefValue parameterId,
      capabilityConstraintValue constraint,
      implIdValue implId,
      maybeValue methodIdValue methodId
    ]

evidenceParameterRefValue :: TypedEvidenceParameterRef -> RuntimeValue
evidenceParameterRefValue (TypedEvidenceParameterRef owner parameterId) =
  constructor
    "TypedEvidenceParameterRef"
    [binderIdValue owner, evidenceParameterIdValue parameterId]

evidenceCandidateValue :: TypedEvidenceCandidate -> RuntimeValue
evidenceCandidateValue (TypedEvidenceCandidate implId methodId) =
  constructor
    "TypedEvidenceCandidate"
    [implIdValue implId, maybeValue methodIdValue methodId]

implIdValue :: TypedImplId -> RuntimeValue
implIdValue (TypedImplId modulePath capability arguments) =
  constructor
    "TypedImplId"
    [textListValue modulePath, coreNameValue capability, listValue typeValue arguments]

methodIdValue :: TypedMethodId -> RuntimeValue
methodIdValue (TypedMethodId implId method) =
  constructor "TypedMethodId" [implIdValue implId, VText method]

typeValue :: TypedType -> RuntimeValue
typeValue typeValue' =
  case typeValue' of
    TypedIntType -> nullary "TypedIntType"
    TypedFloatType -> nullary "TypedFloatType"
    TypedNumericType numericType -> constructor "TypedNumericType" [numericTypeValue numericType]
    TypedBoolType -> nullary "TypedBoolType"
    TypedCharType -> nullary "TypedCharType"
    TypedTextType -> nullary "TypedTextType"
    TypedListType elementType -> constructor "TypedListType" [typeValue elementType]
    TypedTupleType elementTypes -> constructor "TypedTupleType" [listValue typeValue elementTypes]
    TypedDataType name arguments ->
      constructor "TypedDataType" [coreNameValue name, listValue typeValue arguments]
    TypedFunctionType argument result ->
      constructor "TypedFunctionType" [typeValue argument, typeValue result]
    TypedTypeParameterType parameterId ->
      constructor "TypedTypeParameterType" [typeParameterIdValue parameterId]

numericTypeValue :: TypedNumericType -> RuntimeValue
numericTypeValue = nullary . numericTypeName

numericTypeName :: TypedNumericType -> Text
numericTypeName numericType =
  case numericType of
    TypedInt8Type -> "TypedInt8Type"
    TypedInt16Type -> "TypedInt16Type"
    TypedInt32Type -> "TypedInt32Type"
    TypedInt64Type -> "TypedInt64Type"
    TypedUInt8Type -> "TypedUInt8Type"
    TypedUInt16Type -> "TypedUInt16Type"
    TypedUInt32Type -> "TypedUInt32Type"
    TypedUInt64Type -> "TypedUInt64Type"
    TypedFloat16Type -> "TypedFloat16Type"
    TypedFloat32Type -> "TypedFloat32Type"
    TypedFloat64Type -> "TypedFloat64Type"

recipeValue :: TypedRepresentationRecipe -> RuntimeValue
recipeValue recipe =
  case recipe of
    TypedUnitRecipe -> nullary "TypedUnitRecipe"
    TypedBoolRecipe -> nullary "TypedBoolRecipe"
    TypedSignedIntegerRecipe width ->
      constructor "TypedSignedIntegerRecipe" [runtimeIntValue width]
    TypedUnsignedIntegerRecipe width ->
      constructor "TypedUnsignedIntegerRecipe" [runtimeIntValue width]
    TypedFloatRecipe width -> constructor "TypedFloatRecipe" [runtimeIntValue width]
    TypedCharRecipe -> nullary "TypedCharRecipe"
    TypedManagedTextRecipe -> nullary "TypedManagedTextRecipe"
    TypedManagedListRecipe elementRecipe ->
      constructor "TypedManagedListRecipe" [recipeValue elementRecipe]
    TypedManagedProductRecipe elementRecipes ->
      constructor "TypedManagedProductRecipe" [listValue recipeValue elementRecipes]
    TypedManagedVariantRecipe name arguments ->
      constructor
        "TypedManagedVariantRecipe"
        [coreNameValue name, listValue typeValue arguments]
    TypedClosureRecipe parameters result ->
      constructor
        "TypedClosureRecipe"
        [listValue recipeValue parameters, recipeValue result]
    TypedRepresentationParameterRecipe parameterId ->
      constructor "TypedRepresentationParameterRecipe" [typeParameterIdValue parameterId]

operatorRefValue :: TypedOperatorRef -> RuntimeValue
operatorRefValue operator =
  case operator of
    TypedBuiltinOperator symbol -> constructor "TypedBuiltinOperator" [VText symbol]
    TypedResolvedOperator name symbol ->
      constructor "TypedResolvedOperator" [coreNameValue name, VText symbol]

coreNameValue :: TypedCoreName -> RuntimeValue
coreNameValue name =
  case name of
    TypedUnresolvedSourceName source -> constructor "TypedUnresolvedSourceName" [VText source]
    TypedUnresolvedQualifiedName qualifier member ->
      constructor "TypedUnresolvedQualifiedName" [VText qualifier, VText member]
    TypedResolvedName origin namespace identifier ->
      constructor
        "TypedResolvedName"
        [nameOriginValue origin, nameNamespaceValue namespace, VText identifier]
    TypedBuiltinName identifier -> constructor "TypedBuiltinName" [VText identifier]
    TypedGeneratedName kind -> constructor "TypedGeneratedName" [generatedNameKindValue kind]

nameOriginValue :: TypedNameOrigin -> RuntimeValue
nameOriginValue origin =
  case origin of
    TypedCurrentModule -> nullary "TypedCurrentModule"
    TypedImportedModule modulePath -> constructor "TypedImportedModule" [textListValue modulePath]
    TypedAmbientPrelude -> nullary "TypedAmbientPrelude"

nameNamespaceValue :: TypedNameNamespace -> RuntimeValue
nameNamespaceValue namespace =
  nullary $ case namespace of
    TypedValueNamespace -> "TypedValueNamespace"
    TypedConstructorNamespace -> "TypedConstructorNamespace"
    TypedTypeNamespace -> "TypedTypeNamespace"
    TypedCapabilityNamespace -> "TypedCapabilityNamespace"

generatedNameKindValue :: TypedGeneratedNameKind -> RuntimeValue
generatedNameKindValue kind =
  case kind of
    TypedLambdaPatternArgument index ->
      constructor "TypedLambdaPatternArgument" [runtimeIntValue index]
    TypedOperatorBinding symbol -> constructor "TypedOperatorBinding" [VText symbol]
    TypedOperatorSectionFunction -> nullary "TypedOperatorSectionFunction"
    TypedOperatorSectionLeft -> nullary "TypedOperatorSectionLeft"
    TypedOperatorSectionRight -> nullary "TypedOperatorSectionRight"

typeParameterIdValue :: TypedTypeParameterId -> RuntimeValue
typeParameterIdValue (TypedTypeParameterId parameterId) =
  constructor "TypedTypeParameterId" [runtimeIntValue parameterId]

evidenceParameterIdValue :: TypedEvidenceParameterId -> RuntimeValue
evidenceParameterIdValue (TypedEvidenceParameterId parameterId) =
  constructor "TypedEvidenceParameterId" [runtimeIntValue parameterId]

binderIdValue :: TypedBinderId -> RuntimeValue
binderIdValue (TypedBinderId (modulePath, lexicalPath, name)) =
  constructor
    "TypedBinderId"
    [textListValue modulePath, listValue runtimeIntValue lexicalPath, coreNameValue name]

spanValueOf :: TypedSpan -> RuntimeValue
spanValueOf (TypedSpan line column) =
  constructor "TypedSpan" [runtimeIntValue line, runtimeIntValue column]

validationFailureValue :: TypedCoreValidationFailure -> RuntimeValue
validationFailureValue (TypedCoreValidationFailure path kind detail) =
  constructor
    "TypedCoreValidationFailure"
    [validationPathValue path, validationKindValue kind, validationDetailValue detail]

validationPathValue :: TypedCoreValidationPath -> RuntimeValue
validationPathValue path =
  case path of
    TypedProgramPath -> nullary "TypedProgramPath"
    TypedPreludePath -> nullary "TypedPreludePath"
    TypedModulePath modulePath -> constructor "TypedModulePath" [textListValue modulePath]
    TypedInterfacePath modulePath -> constructor "TypedInterfacePath" [textListValue modulePath]
    TypedStatementPath modulePath statementPath ->
      constructor "TypedStatementPath" [textListValue modulePath, listValue runtimeIntValue statementPath]
    TypedExpressionPath modulePath statementPath expressionPath ->
      constructor
        "TypedExpressionPath"
        [textListValue modulePath, listValue runtimeIntValue statementPath, listValue runtimeIntValue expressionPath]
    TypedPatternPath modulePath statementPath patternPath ->
      constructor
        "TypedPatternPath"
        [textListValue modulePath, listValue runtimeIntValue statementPath, listValue runtimeIntValue patternPath]

validationKindValue :: TypedCoreValidationKind -> RuntimeValue
validationKindValue = nullary . validationKindName

validationKindTable :: [(Text, TypedCoreValidationKind)]
validationKindTable =
  [(validationKindName kind, kind) | kind <- [minBound .. maxBound]]

validationKindName :: TypedCoreValidationKind -> Text
validationKindName kind =
  case kind of
    TypedUnresolvedName -> "TypedUnresolvedName"
    TypedInvalidSourcePath -> "TypedInvalidSourcePath"
    TypedInvalidSpan -> "TypedInvalidSpan"
    TypedDuplicateModule -> "TypedDuplicateModule"
    TypedUnknownEntryModule -> "TypedUnknownEntryModule"
    TypedDuplicateBinder -> "TypedDuplicateBinder"
    TypedDuplicateDeclaration -> "TypedDuplicateDeclaration"
    TypedUnknownBinder -> "TypedUnknownBinder"
    TypedDuplicateTypeParameter -> "TypedDuplicateTypeParameter"
    TypedInvalidTypeParameterOrder -> "TypedInvalidTypeParameterOrder"
    TypedUnboundTypeParameter -> "TypedUnboundTypeParameter"
    TypedUnboundRepresentationParameter -> "TypedUnboundRepresentationParameter"
    TypedInvalidRepresentationWidth -> "TypedInvalidRepresentationWidth"
    TypedTypeRepresentationMismatch -> "TypedTypeRepresentationMismatch"
    TypedApplicationFunctionMismatch -> "TypedApplicationFunctionMismatch"
    TypedApplicationArgumentMismatch -> "TypedApplicationArgumentMismatch"
    TypedApplicationResultMismatch -> "TypedApplicationResultMismatch"
    TypedConditionalConditionMismatch -> "TypedConditionalConditionMismatch"
    TypedConditionalBranchMismatch -> "TypedConditionalBranchMismatch"
    TypedPatternScrutineeMismatch -> "TypedPatternScrutineeMismatch"
    TypedPatternGuardMismatch -> "TypedPatternGuardMismatch"
    TypedPatternArmResultMismatch -> "TypedPatternArmResultMismatch"
    TypedOrPatternBinderMismatch -> "TypedOrPatternBinderMismatch"
    TypedDuplicateEvidenceParameter -> "TypedDuplicateEvidenceParameter"
    TypedInvalidEvidenceParameterOrder -> "TypedInvalidEvidenceParameterOrder"
    TypedInstantiationMismatch -> "TypedInstantiationMismatch"
    TypedMissingEvidence -> "TypedMissingEvidence"
    TypedDuplicateEvidence -> "TypedDuplicateEvidence"
    TypedAmbiguousEvidence -> "TypedAmbiguousEvidence"
    TypedInvisibleName -> "TypedInvisibleName"
    TypedInvisibleImpl -> "TypedInvisibleImpl"
    TypedMethodSelectionMismatch -> "TypedMethodSelectionMismatch"
    TypedBindingValueMismatch -> "TypedBindingValueMismatch"
    TypedLambdaResultMismatch -> "TypedLambdaResultMismatch"
    TypedLiteralTypeMismatch -> "TypedLiteralTypeMismatch"
    TypedCollectionShapeMismatch -> "TypedCollectionShapeMismatch"
    TypedDataTypeMismatch -> "TypedDataTypeMismatch"
    TypedPatternShapeMismatch -> "TypedPatternShapeMismatch"
    TypedBlockResultMismatch -> "TypedBlockResultMismatch"
    TypedModuleResultMismatch -> "TypedModuleResultMismatch"
    TypedDataRecipeMismatch -> "TypedDataRecipeMismatch"
    TypedCallableRecipeMismatch -> "TypedCallableRecipeMismatch"
    TypedModuleInterfaceMismatch -> "TypedModuleInterfaceMismatch"

validationDetailValue :: TypedCoreValidationDetail -> RuntimeValue
validationDetailValue detail =
  case detail of
    TypedNoValidationDetail -> nullary "TypedNoValidationDetail"
    TypedTextDetail textValue -> constructor "TypedTextDetail" [VText textValue]
    TypedIndexDetail index -> constructor "TypedIndexDetail" [runtimeIntValue index]
    TypedArityDetail expected actual ->
      constructor "TypedArityDetail" [runtimeIntValue expected, runtimeIntValue actual]
    TypedNameDetail name -> constructor "TypedNameDetail" [coreNameValue name]
    TypedBinderDetail binder -> constructor "TypedBinderDetail" [binderIdValue binder]
    TypedTypeDetail expected actual ->
      constructor "TypedTypeDetail" [typeValue expected, typeValue actual]
    TypedRecipeDetail expected actual ->
      constructor "TypedRecipeDetail" [recipeValue expected, recipeValue actual]
    TypedTypeParameterDetail parameterId ->
      constructor "TypedTypeParameterDetail" [typeParameterIdValue parameterId]
    TypedEvidenceParameterDetail parameterId ->
      constructor "TypedEvidenceParameterDetail" [evidenceParameterIdValue parameterId]
    TypedImplDetail implId -> constructor "TypedImplDetail" [implIdValue implId]

decodeValidationFailure :: RuntimeValue -> Either Text TypedCoreValidationFailure
decodeValidationFailure value = do
  fields <- expectNamedConstructor "validation failure" "TypedCoreValidationFailure" 3 value
  case fields of
    [pathValue, kindValue, detailValue] ->
      TypedCoreValidationFailure
        <$> decodeValidationPath pathValue
        <*> decodeValidationKind kindValue
        <*> decodeValidationDetail detailValue
    _ -> impossibleArity "TypedCoreValidationFailure"

decodeValidationPath :: RuntimeValue -> Either Text TypedCoreValidationPath
decodeValidationPath value = do
  (name, arguments) <- expectConstructor "validation path" value
  case name of
    "TypedProgramPath" -> expectNullary name arguments TypedProgramPath
    "TypedPreludePath" -> expectNullary name arguments TypedPreludePath
    "TypedModulePath" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [modulePath] -> TypedModulePath <$> decodeTextList "module path" modulePath
        _ -> impossibleArity name
    "TypedInterfacePath" -> do
      fields <- expectArity name 1 arguments
      case fields of
        [modulePath] -> TypedInterfacePath <$> decodeTextList "interface module path" modulePath
        _ -> impossibleArity name
    "TypedStatementPath" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [modulePath, statementPath] ->
          TypedStatementPath <$> decodeTextList "statement module path" modulePath <*> decodeIntList "statement path" statementPath
        _ -> impossibleArity name
    "TypedExpressionPath" -> do
      fields <- expectArity name 3 arguments
      case fields of
        [modulePath, statementPath, expressionPath] ->
          TypedExpressionPath
            <$> decodeTextList "expression module path" modulePath
            <*> decodeIntList "expression statement path" statementPath
            <*> decodeIntList "expression path" expressionPath
        _ -> impossibleArity name
    "TypedPatternPath" -> do
      fields <- expectArity name 3 arguments
      case fields of
        [modulePath, statementPath, patternPath] ->
          TypedPatternPath
            <$> decodeTextList "pattern module path" modulePath
            <*> decodeIntList "pattern statement path" statementPath
            <*> decodeIntList "pattern path" patternPath
        _ -> impossibleArity name
    _ -> Left ("unknown validation path constructor '" <> name <> "'")

decodeValidationKind :: RuntimeValue -> Either Text TypedCoreValidationKind
decodeValidationKind value = do
  (name, arguments) <- expectConstructor "validation kind" value
  kind <-
    maybe
      (Left ("unknown validation kind constructor '" <> name <> "'"))
      Right
      (lookup name validationKindTable)
  expectNullary name arguments kind

decodeValidationDetail :: RuntimeValue -> Either Text TypedCoreValidationDetail
decodeValidationDetail value = do
  (name, arguments) <- expectConstructor "validation detail" value
  case name of
    "TypedNoValidationDetail" -> expectNullary name arguments TypedNoValidationDetail
    "TypedTextDetail" -> decodeDetail1 name TypedTextDetail (decodeText "validation text") arguments
    "TypedIndexDetail" -> decodeDetail1 name TypedIndexDetail (decodeInt "validation index") arguments
    "TypedArityDetail" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [expected, actual] -> TypedArityDetail <$> decodeInt "expected arity" expected <*> decodeInt "actual arity" actual
        _ -> impossibleArity name
    "TypedNameDetail" -> decodeDetail1 name TypedNameDetail decodeCoreName arguments
    "TypedBinderDetail" -> decodeDetail1 name TypedBinderDetail decodeBinderId arguments
    "TypedTypeDetail" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [expected, actual] -> TypedTypeDetail <$> decodeType expected <*> decodeType actual
        _ -> impossibleArity name
    "TypedRecipeDetail" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [expected, actual] -> TypedRecipeDetail <$> decodeRecipe expected <*> decodeRecipe actual
        _ -> impossibleArity name
    "TypedTypeParameterDetail" -> decodeDetail1 name TypedTypeParameterDetail decodeTypeParameterId arguments
    "TypedEvidenceParameterDetail" -> decodeDetail1 name TypedEvidenceParameterDetail decodeEvidenceParameterId arguments
    "TypedImplDetail" -> decodeDetail1 name TypedImplDetail decodeImplId arguments
    _ -> Left ("unknown validation detail constructor '" <> name <> "'")

decodeDetail1 :: Text -> (value -> result) -> (RuntimeValue -> Either Text value) -> [RuntimeValue] -> Either Text result
decodeDetail1 name build decodeValue arguments = do
  fields <- expectArity name 1 arguments
  case fields of
    [field] -> build <$> decodeValue field
    _ -> impossibleArity name

decodeCoreName :: RuntimeValue -> Either Text TypedCoreName
decodeCoreName value = do
  (name, arguments) <- expectConstructor "typed-core name" value
  case name of
    "TypedUnresolvedSourceName" -> decodeDetail1 name TypedUnresolvedSourceName (decodeText "source name") arguments
    "TypedUnresolvedQualifiedName" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [qualifier, member] -> TypedUnresolvedQualifiedName <$> decodeText "name qualifier" qualifier <*> decodeText "qualified member" member
        _ -> impossibleArity name
    "TypedResolvedName" -> do
      fields <- expectArity name 3 arguments
      case fields of
        [origin, namespace, identifier] ->
          TypedResolvedName <$> decodeNameOrigin origin <*> decodeNameNamespace namespace <*> decodeText "resolved identifier" identifier
        _ -> impossibleArity name
    "TypedBuiltinName" -> decodeDetail1 name TypedBuiltinName (decodeText "builtin name") arguments
    "TypedGeneratedName" -> decodeDetail1 name TypedGeneratedName decodeGeneratedNameKind arguments
    _ -> Left ("unknown typed-core name constructor '" <> name <> "'")

decodeNameOrigin :: RuntimeValue -> Either Text TypedNameOrigin
decodeNameOrigin value = do
  (name, arguments) <- expectConstructor "name origin" value
  case name of
    "TypedCurrentModule" -> expectNullary name arguments TypedCurrentModule
    "TypedImportedModule" -> decodeDetail1 name TypedImportedModule (decodeTextList "imported module path") arguments
    "TypedAmbientPrelude" -> expectNullary name arguments TypedAmbientPrelude
    _ -> Left ("unknown name origin constructor '" <> name <> "'")

decodeNameNamespace :: RuntimeValue -> Either Text TypedNameNamespace
decodeNameNamespace value = do
  (name, arguments) <- expectConstructor "name namespace" value
  namespace <-
    case name of
      "TypedValueNamespace" -> Right TypedValueNamespace
      "TypedConstructorNamespace" -> Right TypedConstructorNamespace
      "TypedTypeNamespace" -> Right TypedTypeNamespace
      "TypedCapabilityNamespace" -> Right TypedCapabilityNamespace
      _ -> Left ("unknown name namespace constructor '" <> name <> "'")
  expectNullary name arguments namespace

decodeGeneratedNameKind :: RuntimeValue -> Either Text TypedGeneratedNameKind
decodeGeneratedNameKind value = do
  (name, arguments) <- expectConstructor "generated name kind" value
  case name of
    "TypedLambdaPatternArgument" -> decodeDetail1 name TypedLambdaPatternArgument (decodeInt "lambda pattern argument index") arguments
    "TypedOperatorBinding" -> decodeDetail1 name TypedOperatorBinding (decodeText "operator binding") arguments
    "TypedOperatorSectionFunction" -> expectNullary name arguments TypedOperatorSectionFunction
    "TypedOperatorSectionLeft" -> expectNullary name arguments TypedOperatorSectionLeft
    "TypedOperatorSectionRight" -> expectNullary name arguments TypedOperatorSectionRight
    _ -> Left ("unknown generated name kind constructor '" <> name <> "'")

decodeBinderId :: RuntimeValue -> Either Text TypedBinderId
decodeBinderId value = do
  fields <- expectNamedConstructor "typed binder id" "TypedBinderId" 3 value
  case fields of
    [modulePath, lexicalPath, name] ->
      TypedBinderId <$> ((,,) <$> decodeTextList "binder module path" modulePath <*> decodeIntList "binder lexical path" lexicalPath <*> decodeCoreName name)
    _ -> impossibleArity "TypedBinderId"

decodeTypeParameterId :: RuntimeValue -> Either Text TypedTypeParameterId
decodeTypeParameterId value = do
  fields <- expectNamedConstructor "type parameter id" "TypedTypeParameterId" 1 value
  case fields of
    [parameterId] -> TypedTypeParameterId <$> decodeInt "type parameter id" parameterId
    _ -> impossibleArity "TypedTypeParameterId"

decodeEvidenceParameterId :: RuntimeValue -> Either Text TypedEvidenceParameterId
decodeEvidenceParameterId value = do
  fields <- expectNamedConstructor "evidence parameter id" "TypedEvidenceParameterId" 1 value
  case fields of
    [parameterId] -> TypedEvidenceParameterId <$> decodeInt "evidence parameter id" parameterId
    _ -> impossibleArity "TypedEvidenceParameterId"

decodeType :: RuntimeValue -> Either Text TypedType
decodeType value = do
  (name, arguments) <- expectConstructor "typed type" value
  case name of
    "TypedIntType" -> expectNullary name arguments TypedIntType
    "TypedFloatType" -> expectNullary name arguments TypedFloatType
    "TypedNumericType" -> decodeDetail1 name TypedNumericType decodeNumericType arguments
    "TypedBoolType" -> expectNullary name arguments TypedBoolType
    "TypedCharType" -> expectNullary name arguments TypedCharType
    "TypedTextType" -> expectNullary name arguments TypedTextType
    "TypedListType" -> decodeDetail1 name TypedListType decodeType arguments
    "TypedTupleType" -> decodeDetail1 name TypedTupleType (decodeList "tuple types" decodeType) arguments
    "TypedDataType" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [dataName, typeArguments] -> TypedDataType <$> decodeCoreName dataName <*> decodeList "data type arguments" decodeType typeArguments
        _ -> impossibleArity name
    "TypedFunctionType" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [argument, result] -> TypedFunctionType <$> decodeType argument <*> decodeType result
        _ -> impossibleArity name
    "TypedTypeParameterType" -> decodeDetail1 name TypedTypeParameterType decodeTypeParameterId arguments
    _ -> Left ("unknown typed type constructor '" <> name <> "'")

decodeNumericType :: RuntimeValue -> Either Text TypedNumericType
decodeNumericType value = do
  (name, arguments) <- expectConstructor "numeric type" value
  numericType <-
    case name of
      "TypedInt8Type" -> Right TypedInt8Type
      "TypedInt16Type" -> Right TypedInt16Type
      "TypedInt32Type" -> Right TypedInt32Type
      "TypedInt64Type" -> Right TypedInt64Type
      "TypedUInt8Type" -> Right TypedUInt8Type
      "TypedUInt16Type" -> Right TypedUInt16Type
      "TypedUInt32Type" -> Right TypedUInt32Type
      "TypedUInt64Type" -> Right TypedUInt64Type
      "TypedFloat16Type" -> Right TypedFloat16Type
      "TypedFloat32Type" -> Right TypedFloat32Type
      "TypedFloat64Type" -> Right TypedFloat64Type
      _ -> Left ("unknown numeric type constructor '" <> name <> "'")
  expectNullary name arguments numericType

decodeRecipe :: RuntimeValue -> Either Text TypedRepresentationRecipe
decodeRecipe value = do
  (name, arguments) <- expectConstructor "representation recipe" value
  case name of
    "TypedUnitRecipe" -> expectNullary name arguments TypedUnitRecipe
    "TypedBoolRecipe" -> expectNullary name arguments TypedBoolRecipe
    "TypedSignedIntegerRecipe" -> decodeDetail1 name TypedSignedIntegerRecipe (decodeInt "signed integer width") arguments
    "TypedUnsignedIntegerRecipe" -> decodeDetail1 name TypedUnsignedIntegerRecipe (decodeInt "unsigned integer width") arguments
    "TypedFloatRecipe" -> decodeDetail1 name TypedFloatRecipe (decodeInt "float width") arguments
    "TypedCharRecipe" -> expectNullary name arguments TypedCharRecipe
    "TypedManagedTextRecipe" -> expectNullary name arguments TypedManagedTextRecipe
    "TypedManagedListRecipe" -> decodeDetail1 name TypedManagedListRecipe decodeRecipe arguments
    "TypedManagedProductRecipe" -> decodeDetail1 name TypedManagedProductRecipe (decodeList "product recipes" decodeRecipe) arguments
    "TypedManagedVariantRecipe" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [variantName, typeArguments] -> TypedManagedVariantRecipe <$> decodeCoreName variantName <*> decodeList "variant type arguments" decodeType typeArguments
        _ -> impossibleArity name
    "TypedClosureRecipe" -> do
      fields <- expectArity name 2 arguments
      case fields of
        [parameters, result] -> TypedClosureRecipe <$> decodeList "closure parameter recipes" decodeRecipe parameters <*> decodeRecipe result
        _ -> impossibleArity name
    "TypedRepresentationParameterRecipe" -> decodeDetail1 name TypedRepresentationParameterRecipe decodeTypeParameterId arguments
    _ -> Left ("unknown representation recipe constructor '" <> name <> "'")

decodeImplId :: RuntimeValue -> Either Text TypedImplId
decodeImplId value = do
  fields <- expectNamedConstructor "typed impl id" "TypedImplId" 3 value
  case fields of
    [modulePath, capability, targetTypes] ->
      TypedImplId
        <$> decodeTextList "impl module path" modulePath
        <*> decodeCoreName capability
        <*> decodeList "impl target types" decodeType targetTypes
    _ -> impossibleArity "TypedImplId"

decodeTextList :: Text -> RuntimeValue -> Either Text [Text]
decodeTextList label = decodeList label (decodeText (label <> " element"))

decodeIntList :: Text -> RuntimeValue -> Either Text [Int]
decodeIntList label = decodeList label (decodeInt (label <> " element"))

decodeList :: Text -> (RuntimeValue -> Either Text value) -> RuntimeValue -> Either Text [value]
decodeList label decodeElement value =
  case value of
    VList elements _ -> traverse decodeElement elements
    _ -> Left (label <> " expected a List, got " <> runtimeValueCategory value)

decodeText :: Text -> RuntimeValue -> Either Text Text
decodeText label value =
  case value of
    VText textValue -> Right textValue
    _ -> Left (label <> " expected Text, got " <> runtimeValueCategory value)

decodeInt :: Text -> RuntimeValue -> Either Text Int
decodeInt label value =
  case value of
    VInt integer _
      | integer < toInteger (minBound :: Int) || integer > toInteger (maxBound :: Int) ->
          Left (label <> " is outside the host Int range: " <> Text.pack (show integer))
      | otherwise -> Right (fromInteger integer)
    _ -> Left (label <> " expected Int, got " <> runtimeValueCategory value)

expectConstructor :: Text -> RuntimeValue -> Either Text (Text, [RuntimeValue])
expectConstructor label value =
  case value of
    VConstructor _ _ constructorName _ arguments -> Right (identifierText constructorName, arguments)
    _ -> Left (label <> " expected a constructor, got " <> runtimeValueCategory value)

expectNamedConstructor :: Text -> Text -> Int -> RuntimeValue -> Either Text [RuntimeValue]
expectNamedConstructor label expectedName expectedArity value = do
  (actualName, arguments) <- expectConstructor label value
  if actualName /= expectedName
    then Left (label <> " expected constructor '" <> expectedName <> "', got '" <> actualName <> "'")
    else expectArity expectedName expectedArity arguments

expectArity :: Text -> Int -> [RuntimeValue] -> Either Text [RuntimeValue]
expectArity name expected arguments
  | length arguments == expected = Right arguments
  | otherwise = Left (name <> " expected " <> Text.pack (show expected) <> " field(s), got " <> Text.pack (show (length arguments)))

expectNullary :: Text -> [RuntimeValue] -> value -> Either Text value
expectNullary name arguments value = do
  _ <- expectArity name 0 arguments
  Right value

impossibleArity :: Text -> Either Text value
impossibleArity name = Left ("internal checked-adapter arity mismatch for '" <> name <> "'")

runtimeValueCategory :: RuntimeValue -> Text
runtimeValueCategory value =
  case value of
    VInt {} -> "Int"
    VFloat {} -> "Float"
    VBool {} -> "Bool"
    VChar {} -> "Char"
    VText {} -> "Text"
    VList {} -> "List"
    VTuple {} -> "Tuple"
    VConstructor {} -> "constructor"
    VClosure {} -> "closure"
    VBuiltin {} -> "builtin"
    VOperator {} -> "operator"
    VSectionLeft {} -> "left section"
    VSectionRight {} -> "right section"
    VQualifiedMethod {} -> "qualified method"
    VTyped {} -> "typed value"
    VExplicitTypeApplication {} -> "explicit type application"
    _ -> "runtime value"

constructor :: Text -> [RuntimeValue] -> RuntimeValue
constructor = canonicalConstructor

nullary :: Text -> RuntimeValue
nullary = canonicalNullaryConstructor

listValue :: (value -> RuntimeValue) -> [value] -> RuntimeValue
listValue render values = VList (map render values) Nothing

textListValue :: [Text] -> RuntimeValue
textListValue = listValue VText

maybeValue :: (value -> RuntimeValue) -> Maybe value -> RuntimeValue
maybeValue render maybeInput =
  case maybeInput of
    Nothing -> nullary "Nothing"
    Just value -> constructor "Just" [render value]
