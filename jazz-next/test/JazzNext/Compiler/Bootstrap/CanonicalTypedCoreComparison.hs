{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Bootstrap.CanonicalTypedCoreComparison
  ( canonicalTypedProgramRuntimeValue,
    canonicalTypedCoreOutcomeRuntimeValue,
    canonicalTypedValidationFailuresRuntimeValue,
  )
where

import Data.Text (Text)
import JazzNext.Compiler.Bootstrap.CanonicalValue
  ( canonicalConstructor,
    canonicalNullaryConstructor,
    runtimeIntValue,
  )
import JazzNext.Compiler.Runtime (RuntimeValue (..))
import JazzNext.Compiler.TypedCore

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
    [VText capability, maybeValue VText method, typeValue typeValue']

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
    [ maybeValue evidenceParameterIdValue parameterId,
      capabilityConstraintValue constraint,
      implIdValue implId,
      maybeValue methodIdValue methodId
    ]

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
    TypedStatementPath modulePath statementIndex ->
      constructor "TypedStatementPath" [textListValue modulePath, runtimeIntValue statementIndex]
    TypedExpressionPath modulePath statementIndex expressionPath ->
      constructor
        "TypedExpressionPath"
        [textListValue modulePath, runtimeIntValue statementIndex, listValue runtimeIntValue expressionPath]
    TypedPatternPath modulePath statementIndex patternPath ->
      constructor
        "TypedPatternPath"
        [textListValue modulePath, runtimeIntValue statementIndex, listValue runtimeIntValue patternPath]

validationKindValue :: TypedCoreValidationKind -> RuntimeValue
validationKindValue = nullary . validationKindName

validationKindName :: TypedCoreValidationKind -> Text
validationKindName kind =
  case kind of
    TypedUnresolvedName -> "TypedUnresolvedName"
    TypedInvalidSourcePath -> "TypedInvalidSourcePath"
    TypedDuplicateModule -> "TypedDuplicateModule"
    TypedUnknownEntryModule -> "TypedUnknownEntryModule"
    TypedDuplicateBinder -> "TypedDuplicateBinder"
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
    TypedInvisibleImpl -> "TypedInvisibleImpl"
    TypedMethodSelectionMismatch -> "TypedMethodSelectionMismatch"
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
