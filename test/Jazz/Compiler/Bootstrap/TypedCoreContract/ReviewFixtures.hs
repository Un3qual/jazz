{-# LANGUAGE OverloadedStrings #-}

-- | Domain and review-regression Typed Core programs.
module Jazz.Compiler.Bootstrap.TypedCoreContract.ReviewFixtures where

import Data.List (zip5)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinOwnership (..),
    BuiltinSymbol (..),
    allBuiltinSymbols,
    builtinSymbolKernelName,
    builtinSymbolName,
    builtinSymbolOwnership,
  )
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)

callableRepresentationSubstitutionProgram :: TypedProgram
callableRepresentationSubstitutionProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    [ TypedLetStatement owner name span1 scheme (polymorphicIdentityExpression modulePath [0] parameter),
      expressionStatement 2 expression
    ]
    emptyInterface
    instantiatedInfo
    modulePath
  where
    fixture = "review-callable-representation-substitution"
    modulePath = fixtureModulePath fixture
    name = fixtureValueName "identity"
    owner = binder modulePath [0] name
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    scheme =
      fixtureClosureScheme
        owner
        [parameter]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    callableType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)
    callableRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe]
        (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    instantiation = TypedInstantiation owner [TypedTypeArgument parameter callableType] Nothing
    instantiatedInfo =
      TypedNodeInfo
        (TypedFunctionType callableType callableType)
        (TypedClosureRecipe [callableRecipe] callableRecipe)
        [instantiation]
        []
    expression = fixtureBoundVariableExpr owner instantiatedInfo name

lambdaCallableParameterRecipeProgram :: TypedProgram
lambdaCallableParameterRecipeProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    [expressionStatement 1 lambda]
    emptyInterface
    lambdaInfo
    modulePath
  where
    fixture = "review-lambda-callable-parameter-recipe"
    modulePath = fixtureModulePath fixture
    parameterName = fixtureValueName "function"
    parameterBinder = binder modulePath [0] parameterName
    parameterType =
      TypedFunctionType
        TypedBoolType
        (TypedFunctionType TypedCharType TypedTextType)
    parameterRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe]
        (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)
    parameterInfo = info parameterType parameterRecipe
    lambdaInfo =
      info
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    lambda =
      TypedLambdaExpr
        lambdaInfo
        parameterBinder
        parameterName
        (fixtureBoundVariableExpr parameterBinder parameterInfo parameterName)

applicationScalarAliasProgram :: TypedProgram
applicationScalarAliasProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    [ intBinding,
      expressionStatement 2 intApplication,
      floatBinding,
      expressionStatement 4 floatApplication
    ]
    emptyInterface
    floatAliasInfo
    modulePath
  where
    fixture = "review-application-scalar-alias"
    modulePath = fixtureModulePath fixture
    int64Type = TypedNumericType TypedInt64Type
    int64Recipe = TypedSignedIntegerRecipe 64
    float64Type = TypedNumericType TypedFloat64Type
    float64Recipe = TypedFloatRecipe 64
    floatAliasInfo = info TypedFloatType float64Recipe
    (intBinding, intApplication) =
      aliasApplication
        0
        "identityInt64"
        int64Type
        TypedIntType
        int64Recipe
        (TypedIntegerLiteral "1")
    (floatBinding, floatApplication) =
      aliasApplication
        2
        "identityFloat64"
        float64Type
        TypedFloatType
        float64Recipe
        (TypedFractionalLiteral "1" "5" Nothing)

    aliasApplication statementIndex nameText explicitType aliasType recipe literal =
      let name = fixtureValueName nameText
          owner = binder modulePath [statementIndex] name
          argumentName = fixtureValueName (nameText <> "Argument")
          argumentOwner = binder modulePath [statementIndex, 0] argumentName
          explicitInfo = info explicitType recipe
          functionType = TypedFunctionType explicitType explicitType
          functionRecipe = TypedClosureRecipe [recipe] recipe
          functionInfo = info functionType functionRecipe
          scheme = fixtureScheme owner [] [] [] functionType functionRecipe
          binding =
            TypedLetStatement
              owner
              name
              span1
              scheme
              (TypedLambdaExpr functionInfo argumentOwner argumentName (fixtureBoundVariableExpr argumentOwner explicitInfo argumentName))
          aliasInfo = info aliasType recipe
          application =
            TypedApplyExpr
              aliasInfo
              (fixtureBoundVariableExpr owner functionInfo name)
              (TypedLiteralExpr aliasInfo literal)
       in (binding, application)

droppedDeferredEvidenceProgram :: TypedProgram
droppedDeferredEvidenceProgram =
  targetCandidateApplicationProgram
    "review-dropped-deferred-evidence"
    False

reorderedDeferredEvidenceProgram :: TypedProgram
reorderedDeferredEvidenceProgram =
  TypedProgram Nothing [providerModule, entryModule] entryPath
  where
    fixture = "review-reordered-deferred-evidence"
    providerPath = fixtureLibraryPath "ReorderedDeferredEvidence"
    entryPath = fixtureModulePath fixture
    parameter = TypedTypeParameterId 0
    providerCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Build"
    importedCapabilityName =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        "Build"
    providerMethodName =
      resolved TypedCurrentModule TypedValueNamespace "build"
    methodOwner = binder providerPath [0, 0] providerMethodName
    genericMethodType =
      TypedFunctionType
        TypedBoolType
        (TypedFunctionType (TypedTypeParameterType parameter) TypedBoolType)
    genericMethodRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe, TypedRepresentationParameterRecipe parameter]
        TypedBoolRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        providerCapabilityName
        [parameter]
        [ TypedMethodSignature
            providerMethodName
            span1
            ( fixtureScheme
                methodOwner
                []
                []
                []
                genericMethodType
                genericMethodRecipe
            )
        ]
    providerImpl =
      TypedImplId providerPath providerCapabilityName [TypedTextType]
    importedProviderImpl =
      TypedImplId providerPath importedCapabilityName [TypedTextType]
    localImpl =
      TypedImplId entryPath importedCapabilityName [TypedTextType]
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/ReorderedDeferredEvidence.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Build"]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface classDeclaration]
            [TypedImplInterface providerImpl]
        )
        [ TypedClassStatement classDeclaration,
          TypedImplStatement
            ( TypedImplDeclaration
                span1
                providerImpl
                [methodDefinition providerPath [1, 0] providerImpl]
            )
        ]
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 providerPath Nothing Nothing]
        []
        emptyInterface
        [ TypedImplStatement
            ( TypedImplDeclaration
                span1
                localImpl
                [methodDefinition entryPath [0, 0] localImpl]
            ),
          expressionStatement 1 expression
        ]
        resultInfo
    specializedMethodType =
      TypedFunctionType TypedBoolType (TypedFunctionType TypedTextType TypedBoolType)
    specializedMethodRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe, TypedManagedTextRecipe]
        TypedBoolRecipe
    specializedMethodInfo =
      info specializedMethodType specializedMethodRecipe
    textToBoolType = TypedFunctionType TypedTextType TypedBoolType
    textToBoolRecipe =
      TypedClosureRecipe [TypedManagedTextRecipe] TypedBoolRecipe
    textToBoolInfo = info textToBoolType textToBoolRecipe
    flagName = resolved TypedCurrentModule TypedValueNamespace "flag"
    targetName = resolved TypedCurrentModule TypedValueNamespace "target"
    methodDefinition modulePath methodPath implId =
      TypedMethodDefinition
        (TypedMethodId implId "build")
        (binder modulePath methodPath providerMethodName)
        providerMethodName
        span1
        ( TypedLambdaExpr
            specializedMethodInfo
            (binder modulePath (methodPath <> [0]) flagName)
            flagName
            ( TypedLambdaExpr
                textToBoolInfo
                (binder modulePath (methodPath <> [0, 0]) targetName)
                targetName
                trueExpr
            )
        )
    constraint =
      TypedCapabilityConstraint
        importedCapabilityName
        (Just qualifiedMethodKey)
        TypedTextType
    qualifiedMethodKey =
      Text.intercalate "::" (providerPath <> ["Build", "build"])
    candidates =
      [ TypedEvidenceCandidate
          importedProviderImpl
          (Just (TypedMethodId importedProviderImpl "build")),
        TypedEvidenceCandidate
          localImpl
          (Just (TypedMethodId localImpl "build"))
      ]
    functionInfo =
      TypedNodeInfo
        specializedMethodType
        specializedMethodRecipe
        []
        [TypedEvidenceCandidates constraint candidates]
    intermediateInfo =
      TypedNodeInfo
        textToBoolType
        textToBoolRecipe
        []
        [TypedEvidenceCandidates constraint (reverse candidates)]
    resultInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        []
        [ TypedSelectedEvidence
            ( TypedEvidenceUse
                Nothing
                constraint
                importedProviderImpl
                (Just (TypedMethodId importedProviderImpl "build"))
            )
        ]
    intermediate =
      TypedApplyExpr
        intermediateInfo
        ( fixtureVariableExpr
            functionInfo
            (TypedBuiltinName qualifiedMethodKey)
        )
        trueExpr
    expression =
      TypedApplyExpr
        resultInfo
        intermediate
        (TypedLiteralExpr textInfo (TypedTextLiteral "target"))

forgedEvidenceCapabilityName :: TypedCoreName
forgedEvidenceCapabilityName =
  resolved
    (TypedImportedModule (fixtureModulePath "review-forged-evidence-capability"))
    TypedCapabilityNamespace
    "Check"

forgedEvidenceCapabilityProgram :: TypedProgram
forgedEvidenceCapabilityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-forged-evidence-capability"
    modulePath = fixtureModulePath fixture
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Check"
    methodName =
      resolved TypedCurrentModule TypedValueNamespace "check"
    methodOwner = binder modulePath [0, 0] methodName
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        [TypedMethodSignature methodName span1 (monoScheme methodOwner)]
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    methodDefinition =
      TypedMethodDefinition
        (TypedMethodId implId "check")
        (binder modulePath [1, 0] methodName)
        methodName
        span1
        trueExpr
    constraint =
      TypedCapabilityConstraint
        forgedEvidenceCapabilityName
        (Just qualifiedMethodKey)
        TypedBoolType
    qualifiedMethodKey =
      Text.intercalate "::" (modulePath <> ["Check", "check"])
    evidenceUse =
      TypedEvidenceUse
        Nothing
        constraint
        implId
        (Just (TypedMethodId implId "check"))
    expression =
      fixtureVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBuiltinName qualifiedMethodKey)
    statements =
      [ TypedClassStatement classDeclaration,
        TypedImplStatement
          (TypedImplDeclaration span1 implId [methodDefinition]),
        expressionStatement 2 expression,
        expressionStatement 3 trueExpr
      ]

emptyMonomorphicValueOwner :: TypedBinderId
emptyMonomorphicValueOwner =
  binder
    (fixtureModulePath "review-empty-monomorphic-instantiation")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "item")

emptyMonomorphicConstructorOwner :: TypedBinderId
emptyMonomorphicConstructorOwner =
  binder
    (fixtureModulePath "review-empty-monomorphic-instantiation")
    [1, 0]
    (resolved TypedCurrentModule TypedConstructorNamespace "Flag")

emptyMonomorphicInstantiationProgram :: TypedProgram
emptyMonomorphicInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-empty-monomorphic-instantiation"
    modulePath = fixtureModulePath fixture
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    valueInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [TypedInstantiation emptyMonomorphicValueOwner [] Nothing]
        []
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"
    constructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Flag"
    constructorType = TypedDataType dataName []
    constructorInfo =
      TypedNodeInfo
        constructorType
        (TypedManagedVariantRecipe dataName [])
        [TypedInstantiation emptyMonomorphicConstructorOwner [] Nothing]
        []
    declaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [ TypedConstructorDeclaration
            emptyMonomorphicConstructorOwner
            constructorName
            []
            []
        ]
    statements =
      [ TypedLetStatement
          emptyMonomorphicValueOwner
          valueName
          span1
          (monoScheme emptyMonomorphicValueOwner)
          trueExpr,
        TypedDataStatement declaration,
        expressionStatement 2 (fixtureVariableExpr valueInfo valueName),
        expressionStatement
          3
          (fixtureVariableExpr constructorInfo constructorName),
        expressionStatement 4 trueExpr
      ]

retainedClassMetadataFailure :: Text -> TypedCoreValidationFailure
retainedClassMetadataFailure fixture =
  TypedCoreValidationFailure
    (TypedInterfacePath (fixtureModulePath fixture))
    TypedModuleInterfaceMismatch
    (TypedNameDetail retainedClassMetadataName)

retainedClassMetadataName :: TypedCoreName
retainedClassMetadataName =
  resolved
    (TypedImportedModule retainedClassMetadataProviderPath)
    TypedCapabilityNamespace
    "Display"

retainedClassMetadataProviderPath :: [Text]
retainedClassMetadataProviderPath =
  fixtureLibraryPath "RetainedClassMetadataProvider"

invalidRetainedClassSpanProgram :: TypedProgram
invalidRetainedClassSpanProgram =
  retainedClassMetadataProgram
    "review-invalid-retained-class-span"
    invalidSpan
    False

duplicateRetainedClassMethodProgram :: TypedProgram
duplicateRetainedClassMethodProgram =
  retainedClassMetadataProgram
    "review-duplicate-retained-class-method"
    span1
    True

retainedClassMetadataProgram :: Text -> TypedSpan -> Bool -> TypedProgram
retainedClassMetadataProgram fixture retainedSpan duplicateMethod =
  TypedProgram Nothing [providerModule, facadeModule] facadePath
  where
    providerPath = retainedClassMetadataProviderPath
    facadePath = fixtureModulePath fixture
    parameter = TypedTypeParameterId 0
    localClassName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Display"
    localMethodName =
      resolved TypedCurrentModule TypedValueNamespace "display"
    methodOwner = binder providerPath [0, 0] localMethodName
    methodScheme = fixtureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    localMethod = TypedMethodSignature localMethodName span1 methodScheme
    localClass =
      TypedClassDeclaration span1 localClassName [parameter] [localMethod]
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/RetainedClassMetadataProvider.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Display"]
        (TypedModuleInterface [] [] [TypedClassInterface localClass] [])
        [TypedClassStatement localClass]
        unitInfo
    retainedMethodName =
      resolved
        (TypedImportedModule providerPath)
        TypedValueNamespace
        "display"
    retainedMethod =
      TypedMethodSignature retainedMethodName span1 methodScheme
    retainedMethods
      | duplicateMethod = [retainedMethod, retainedMethod]
      | otherwise = [retainedMethod]
    retainedClass =
      TypedClassDeclaration
        retainedSpan
        retainedClassMetadataName
        [parameter]
        retainedMethods
    facadeModule =
      typedModule
        facadePath
        relativeSource
        [TypedResolvedImport span1 providerPath Nothing (Just ["Display"])]
        []
        (TypedModuleInterface [] [] [TypedClassInterface retainedClass] [])
        []
        unitInfo

negativeBinderPathOwner :: TypedBinderId
negativeBinderPathOwner =
  binder
    (fixtureModulePath "review-negative-binder-path")
    [-1]
    (fixtureValueName "answer")

negativeBinderPathProgram :: TypedProgram
negativeBinderPathProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface unitInfo modulePath
  where
    fixture = "review-negative-binder-path"
    modulePath = fixtureModulePath fixture
    valueName = fixtureValueName "answer"
    statement =
      TypedLetStatement
        negativeBinderPathOwner
        valueName
        span1
        (monoScheme negativeBinderPathOwner)
        trueExpr

wrongDataNamespaceName :: TypedCoreName
wrongDataNamespaceName =
  resolved TypedCurrentModule TypedValueNamespace "Box"

wrongDataNamespaceProgram :: TypedProgram
wrongDataNamespaceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-wrong-data-namespace"
    modulePath = fixtureModulePath fixture
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    declaration =
      dataDeclarationWithNullaryConstructor modulePath [0, 0] dataName []
    valueName = fixtureValueName "item"
    owner = binder modulePath [1] valueName
    invalidType = TypedDataType wrongDataNamespaceName []
    invalidRecipe = TypedManagedVariantRecipe wrongDataNamespaceName []
    scheme = fixtureScheme owner [] [] [] invalidType invalidRecipe
    statements =
      [ TypedDataStatement declaration,
        TypedSignatureStatement owner valueName span1 scheme
      ]

wrongConstructorNamespaceName :: TypedCoreName
wrongConstructorNamespaceName =
  resolved TypedCurrentModule TypedValueNamespace "Box"

wrongConstructorNamespaceProgram :: TypedProgram
wrongConstructorNamespaceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-wrong-constructor-namespace"
    modulePath = fixtureModulePath fixture
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    declaration =
      dataDeclarationWithNullaryConstructor modulePath [0, 0] dataName []
    patternValue =
      TypedConstructorPattern boolInfo wrongConstructorNamespaceName []
    expression =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm patternValue Nothing trueExpr]
    statements =
      [TypedDataStatement declaration, expressionStatement 1 expression]

appliedTargetCandidateDeferralProgram :: TypedProgram
appliedTargetCandidateDeferralProgram =
  targetCandidateApplicationProgram
    "review-applied-target-candidate-deferral"
    True

targetCandidateApplicationProgram :: Text -> Bool -> TypedProgram
targetCandidateApplicationProgram fixture retainCandidate =
  singleModuleProgram fixture relativeSource [] statements emptyInterface resultInfo modulePath
  where
    modulePath = fixtureModulePath fixture
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Build"
    methodName = resolved TypedCurrentModule TypedValueNamespace "build"
    methodOwner = binder modulePath [0, 0] methodName
    genericMethodType = TypedFunctionType parameterType boolToBoolType
    genericMethodRecipe =
      TypedClosureRecipe [parameterRecipe, TypedBoolRecipe] TypedBoolRecipe
    methodScheme =
      fixtureScheme
        methodOwner
        []
        []
        []
        genericMethodType
        genericMethodRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    implId = TypedImplId modulePath capabilityName [TypedTextType]
    targetName = resolved TypedCurrentModule TypedValueNamespace "target"
    resultName = resolved TypedCurrentModule TypedValueNamespace "result"
    specializedMethodType = TypedFunctionType TypedTextType boolToBoolType
    specializedMethodRecipe =
      TypedClosureRecipe
        [TypedManagedTextRecipe, TypedBoolRecipe]
        TypedBoolRecipe
    methodExpression =
      TypedLambdaExpr
        (info specializedMethodType specializedMethodRecipe)
        (binder modulePath [1, 0, 0] targetName)
        targetName
        ( TypedLambdaExpr
            boolToBoolInfo
            (binder modulePath [1, 0, 0, 0] resultName)
            resultName
            trueExpr
        )
    methodDefinition =
      TypedMethodDefinition
        (TypedMethodId implId "build")
        (binder modulePath [1, 0] methodName)
        methodName
        span1
        methodExpression
    constraint =
      TypedCapabilityConstraint capabilityName (Just "Build.build") TypedTextType
    candidate =
      TypedEvidenceCandidate implId (Just (TypedMethodId implId "build"))
    selection = TypedEvidenceCandidates constraint [candidate]
    functionInfo =
      TypedNodeInfo specializedMethodType specializedMethodRecipe [] [selection]
    resultInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        []
        (if retainCandidate then [selection] else [])
    expression =
      TypedApplyExpr
        resultInfo
        ( fixtureVariableExpr
            functionInfo
            (TypedBuiltinName "Build::build")
        )
        (TypedLiteralExpr textInfo (TypedTextLiteral "target"))
    statements =
      [ TypedClassStatement classDeclaration,
        TypedImplStatement
          (TypedImplDeclaration span1 implId [methodDefinition]),
        expressionStatement 2 expression
      ]

localCapabilityExportIdentityProgram :: TypedProgram
localCapabilityExportIdentityProgram =
  TypedProgram Nothing [providerModule, entryModule] entryPath
  where
    providerPath = fixtureLibraryPath "CapabilityExportIdentity"
    entryPath = fixtureModulePath "review-local-capability-export-identity"
    parameter = TypedTypeParameterId 0
    providerClassName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Visible"
    providerDeclaration =
      TypedClassDeclaration span1 providerClassName [parameter] []
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/CapabilityExportIdentity.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Visible"]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface providerDeclaration]
            []
        )
        [TypedClassStatement providerDeclaration]
        unitInfo
    localClassName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Visible"
    localDeclaration =
      TypedClassDeclaration span1 localClassName [parameter] []
    retainedClassName =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        "Visible"
    retainedDeclaration =
      TypedClassDeclaration span1 retainedClassName [parameter] []
    entryModule =
      typedModule
        entryPath
        relativeSource
        [ TypedResolvedImport
            span1
            providerPath
            (Just "Library")
            Nothing
        ]
        [TypedModuleExport TypedCapabilityNamespace "Visible"]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface retainedDeclaration]
            []
        )
        [TypedClassStatement localDeclaration]
        unitInfo

duplicateDeferredEvidenceProgram :: TypedProgram
duplicateDeferredEvidenceProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-duplicate-deferred-evidence"
    candidate =
      fixtureRenderCandidate (fixtureRenderImpl ["Prelude"])
    selection =
      TypedEvidenceCandidates fixtureRenderConstraint [candidate]
    expression =
      fixtureVariableExpr
        (TypedNodeInfo builtinMapType builtinMapRecipe [] [selection, selection])
        (TypedBuiltinName "map")

ambiguousValueExportName :: TypedCoreName
ambiguousValueExportName =
  resolved TypedCurrentModule TypedValueNamespace "render"

ambiguousValueExportProgram :: TypedProgram
ambiguousValueExportProgram =
  singleModuleProgram fixture relativeSource exports statements interface boolInfo modulePath
  where
    fixture = "review-ambiguous-value-export"
    modulePath = fixtureModulePath fixture
    valueOwner = binder modulePath [0] ambiguousValueExportName
    valueScheme = monoScheme valueOwner
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Display"
    methodOwner = binder modulePath [1, 0] ambiguousValueExportName
    methodScheme = monoScheme methodOwner
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        [TypedMethodSignature ambiguousValueExportName span1 methodScheme]
    exports = [TypedModuleExport TypedValueNamespace "render"]
    statements =
      [ TypedLetStatement valueOwner ambiguousValueExportName span1 valueScheme trueExpr,
        TypedClassStatement classDeclaration,
        expressionStatement 2 trueExpr
      ]
    interface =
      TypedModuleInterface
        [TypedValueInterface ambiguousValueExportName valueScheme]
        []
        [TypedClassInterface classDeclaration]
        []

unentailedPrimitiveInstantiationProgram :: TypedProgram
unentailedPrimitiveInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-unentailed-primitive-instantiation"
    modulePath = fixtureModulePath fixture
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    numericName = fixtureValueName "numeric"
    numericOwner = binder modulePath [0] numericName
    equalityName = fixtureValueName "equality"
    equalityOwner = binder modulePath [1] equalityName
    outerScheme owner =
      fixtureScheme
        owner
        [parameter]
        []
        []
        (TypedFunctionType parameterType TypedBoolType)
        (TypedClosureRecipe [parameterRecipe] TypedBoolRecipe)
    constrainedOuterScheme owner constraint =
      fixtureScheme
        owner
        [parameter]
        []
        [constraint]
        (TypedFunctionType parameterType TypedBoolType)
        (TypedClosureRecipe [parameterRecipe] TypedBoolRecipe)
    constrainedScheme owner constraint =
      fixtureScheme owner [parameter] [] [constraint] TypedBoolType TypedBoolRecipe
    instantiate owner =
      TypedInstantiation
        owner
        [TypedTypeArgument parameter parameterType]
        Nothing
    outerExpression statementIndex owner name =
      TypedLambdaExpr
        ( info
            (TypedFunctionType parameterType TypedBoolType)
            (TypedClosureRecipe [parameterRecipe] TypedBoolRecipe)
        )
        (binder modulePath [statementIndex, 0] (fixtureValueName "argument"))
        (fixtureValueName "argument")
        ( fixtureVariableExpr
            (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiate owner] [])
            name
        )
    numericOuterName = fixtureValueName "numericOuter"
    numericOuterOwner = binder modulePath [2] numericOuterName
    equalityOuterName = fixtureValueName "equalityOuter"
    equalityOuterOwner = binder modulePath [3] equalityOuterName
    entailedNumericOuterName = fixtureValueName "entailedNumericOuter"
    entailedNumericOuterOwner = binder modulePath [4] entailedNumericOuterName
    entailedEqualityOuterName = fixtureValueName "entailedEqualityOuter"
    entailedEqualityOuterOwner = binder modulePath [5] entailedEqualityOuterName
    integralName = fixtureValueName "integral"
    integralOwner = binder modulePath [6] integralName
    arithmeticOuterName = fixtureValueName "arithmeticOuter"
    arithmeticOuterOwner = binder modulePath [7] arithmeticOuterName
    statements =
      [ TypedLetStatement
          numericOwner
          numericName
          span1
          ( constrainedScheme
              numericOwner
              (TypedNumericPrimitiveConstraint TypedRuntimeArithmeticNumericConstraint parameterType)
          )
          trueExpr,
        TypedLetStatement
          equalityOwner
          equalityName
          span1
          (constrainedScheme equalityOwner (TypedStrictEqualityPrimitiveConstraint parameterType))
          trueExpr,
        TypedLetStatement
          numericOuterOwner
          numericOuterName
          span1
          (outerScheme numericOuterOwner)
          (outerExpression 2 numericOwner numericName),
        TypedLetStatement
          equalityOuterOwner
          equalityOuterName
          span1
          (outerScheme equalityOuterOwner)
          (outerExpression 3 equalityOwner equalityName),
        TypedLetStatement
          entailedNumericOuterOwner
          entailedNumericOuterName
          span1
          ( constrainedOuterScheme
              entailedNumericOuterOwner
              (TypedNumericPrimitiveConstraint TypedIntegralNumericConstraint parameterType)
          )
          (outerExpression 4 numericOwner numericName),
        TypedLetStatement
          entailedEqualityOuterOwner
          entailedEqualityOuterName
          span1
          ( constrainedOuterScheme
              entailedEqualityOuterOwner
              (TypedStrictEqualityPrimitiveConstraint parameterType)
          )
          (outerExpression 5 equalityOwner equalityName),
        TypedLetStatement
          integralOwner
          integralName
          span1
          ( constrainedScheme
              integralOwner
              (TypedNumericPrimitiveConstraint TypedIntegralNumericConstraint parameterType)
          )
          trueExpr,
        TypedLetStatement
          arithmeticOuterOwner
          arithmeticOuterName
          span1
          ( constrainedOuterScheme
              arithmeticOuterOwner
              (TypedNumericPrimitiveConstraint TypedRuntimeArithmeticNumericConstraint parameterType)
          )
          (outerExpression 7 integralOwner integralName),
        expressionStatement 8 trueExpr
      ]

generatedClassMethodName :: TypedCoreName
generatedClassMethodName =
  TypedGeneratedName TypedOperatorSectionFunction

generatedClassMethodNameProgram :: TypedProgram
generatedClassMethodNameProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-generated-class-method-name"
    modulePath = fixtureModulePath fixture
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "GeneratedMethod"
    methodOwner = binder modulePath [0, 0] generatedClassMethodName
    methodScheme = monoScheme methodOwner
    declaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        [TypedMethodSignature generatedClassMethodName span1 methodScheme]
    statements =
      [ TypedClassStatement declaration,
        expressionStatement 1 trueExpr
      ]

singletonOrPatternProgram :: TypedProgram
singletonOrPatternProgram =
  expressionFixtureProgram
    "review-singleton-or-pattern"
    ( TypedPatternCaseExpr
        boolInfo
        trueExpr
        [ TypedCaseArm
            (TypedOrPattern boolInfo [TypedWildcardPattern boolInfo])
            Nothing
            trueExpr
        ]
    )

fractionalPatternProgram :: TypedProgram
fractionalPatternProgram =
  expressionFixtureProgram
    "review-fractional-pattern"
    ( TypedPatternCaseExpr
        boolInfo
        fractionalExpression
        [ TypedCaseArm
            (TypedLiteralPattern fractionalInfo fractionalLiteral)
            Nothing
            trueExpr
        ]
    )
  where
    fractionalInfo =
      info
        (TypedNumericType TypedFloat64Type)
        (TypedFloatRecipe 64)
    fractionalLiteral =
      TypedFractionalLiteral "1" "5" (Just TypedFloat64Type)
    fractionalExpression =
      TypedLiteralExpr fractionalInfo fractionalLiteral

lowercaseTypeName :: TypedCoreName
lowercaseTypeName =
  resolved TypedCurrentModule TypedTypeNamespace "lower"

lowercaseConstructorName :: TypedCoreName
lowercaseConstructorName =
  resolved TypedCurrentModule TypedConstructorNamespace "lowerConstructor"

lowercaseCapabilityName :: TypedCoreName
lowercaseCapabilityName =
  resolved TypedCurrentModule TypedCapabilityNamespace "lowerCapability"

lowercaseConstructorLikeNamesProgram :: TypedProgram
lowercaseConstructorLikeNamesProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-lowercase-constructor-like-names"
    modulePath = fixtureModulePath fixture
    dataDeclaration =
      TypedDataDeclaration
        span1
        lowercaseTypeName
        []
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] lowercaseConstructorName)
            lowercaseConstructorName
            []
            []
        ]
    capabilityDeclaration =
      TypedClassDeclaration
        span1
        lowercaseCapabilityName
        [TypedTypeParameterId 0]
        []
    statements =
      [ TypedDataStatement dataDeclaration,
        TypedClassStatement capabilityDeclaration,
        expressionStatement 2 trueExpr
      ]

duplicateOrderingDataName :: TypedCoreName
duplicateOrderingDataName =
  resolved TypedCurrentModule TypedTypeNamespace "Duplicate"

duplicateOrderingConstructorName :: TypedCoreName
duplicateOrderingConstructorName =
  resolved TypedCurrentModule TypedConstructorNamespace "Duplicate"

duplicateOrderingImplId :: TypedImplId
duplicateOrderingImplId =
  TypedImplId
    (fixtureModulePath "review-duplicate-declaration-ordering")
    (resolved TypedCurrentModule TypedCapabilityNamespace "Marker")
    [TypedBoolType]

duplicateDeclarationOrderingProgram :: TypedProgram
duplicateDeclarationOrderingProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-duplicate-declaration-ordering"
    modulePath = fixtureModulePath fixture
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Marker"
    capabilityDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    dataDeclaration statementIndex =
      TypedDataDeclaration
        span1
        duplicateOrderingDataName
        []
        [ TypedConstructorDeclaration
            (binder modulePath [statementIndex, 0] duplicateOrderingConstructorName)
            duplicateOrderingConstructorName
            []
            []
        ]
    statements =
      [ TypedClassStatement capabilityDeclaration,
        TypedImplStatement (TypedImplDeclaration span1 duplicateOrderingImplId []),
        TypedImplStatement (TypedImplDeclaration span1 duplicateOrderingImplId []),
        TypedDataStatement (dataDeclaration 3),
        TypedDataStatement (dataDeclaration 4),
        expressionStatement 5 trueExpr
      ]

builtinCatalogProgram :: TypedProgram
builtinCatalogProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface terminalInfo modulePath
  where
    fixture = "review-builtin-catalog-parity"
    modulePath = fixtureModulePath fixture
    builtinExpressions =
      [ fixtureVariableExpr (builtinCatalogInfo symbol) (TypedBuiltinName name)
      | symbol <- allBuiltinSymbols,
        name <- builtinAcceptedNames symbol
      ]
    statements =
      zipWith expressionStatement [1 ..] builtinExpressions
    terminalInfo = builtinCatalogInfo BuiltinExit

builtinAcceptedNames :: BuiltinSymbol -> [Text]
builtinAcceptedNames symbol =
  case builtinSymbolOwnership symbol of
    PreludeTarget ->
      [builtinSymbolName symbol, builtinSymbolKernelName symbol]
    KernelIntrinsic ->
      [builtinSymbolKernelName symbol]

builtinCatalogInfo :: BuiltinSymbol -> TypedNodeInfo
builtinCatalogInfo symbol =
  case symbol of
    BuiltinMap -> builtinMapInfo
    BuiltinFilter ->
      info
        ( TypedFunctionType
            (TypedFunctionType TypedBoolType TypedBoolType)
            (TypedFunctionType (TypedListType TypedBoolType) (TypedListType TypedBoolType))
        )
        ( TypedClosureRecipe
            [boolToBoolRecipe]
            ( TypedClosureRecipe
                [TypedManagedListRecipe TypedBoolRecipe]
                (TypedManagedListRecipe TypedBoolRecipe)
            )
        )
    BuiltinHd ->
      functionInfo
        (TypedListType TypedBoolType)
        (TypedManagedListRecipe TypedBoolRecipe)
        TypedBoolType
        TypedBoolRecipe
    BuiltinTl -> boolListTransformInfo
    BuiltinPrint -> boolToBoolInfo
    BuiltinToInt8 -> numericConversionInfo TypedInt8Type (TypedSignedIntegerRecipe 8)
    BuiltinToInt16 -> numericConversionInfo TypedInt16Type (TypedSignedIntegerRecipe 16)
    BuiltinToInt32 -> numericConversionInfo TypedInt32Type (TypedSignedIntegerRecipe 32)
    BuiltinToInt64 -> numericConversionInfo TypedInt64Type (TypedSignedIntegerRecipe 64)
    BuiltinToUInt8 -> numericConversionInfo TypedUInt8Type (TypedUnsignedIntegerRecipe 8)
    BuiltinToUInt16 -> numericConversionInfo TypedUInt16Type (TypedUnsignedIntegerRecipe 16)
    BuiltinToUInt32 -> numericConversionInfo TypedUInt32Type (TypedUnsignedIntegerRecipe 32)
    BuiltinToUInt64 -> numericConversionInfo TypedUInt64Type (TypedUnsignedIntegerRecipe 64)
    BuiltinToFloat16 -> numericConversionInfo TypedFloat16Type (TypedFloatRecipe 16)
    BuiltinToFloat32 -> numericConversionInfo TypedFloat32Type (TypedFloatRecipe 32)
    BuiltinToFloat64 -> numericConversionInfo TypedFloat64Type (TypedFloatRecipe 64)
    BuiltinListPrependRaw ->
      info
        ( TypedFunctionType
            TypedBoolType
            (TypedFunctionType (TypedListType TypedBoolType) (TypedListType TypedBoolType))
        )
        ( TypedClosureRecipe
            [TypedBoolRecipe]
            ( TypedClosureRecipe
                [TypedManagedListRecipe TypedBoolRecipe]
                (TypedManagedListRecipe TypedBoolRecipe)
            )
        )
    BuiltinListReverseRaw -> boolListTransformInfo
    BuiltinCharToUInt32 ->
      functionInfo
        TypedCharType
        TypedCharRecipe
        (TypedNumericType TypedUInt32Type)
        (TypedUnsignedIntegerRecipe 32)
    BuiltinCharFromUInt32Raw ->
      functionInfo
        (TypedNumericType TypedUInt32Type)
        (TypedUnsignedIntegerRecipe 32)
        (TypedListType TypedCharType)
        (TypedManagedListRecipe TypedCharRecipe)
    BuiltinCharIsAlpha -> charPredicateInfo
    BuiltinCharIsAlphaNum -> charPredicateInfo
    BuiltinCharIsDigit -> charPredicateInfo
    BuiltinCharIsSpace -> charPredicateInfo
    BuiltinCharIsHexDigit -> charPredicateInfo
    BuiltinCharIsLower -> charPredicateInfo
    BuiltinCharIsUpper -> charPredicateInfo
    BuiltinCharToLower -> charTransformInfo
    BuiltinCharToUpper -> charTransformInfo
    BuiltinTextLength ->
      functionInfo
        TypedTextType
        TypedManagedTextRecipe
        TypedIntType
        (TypedSignedIntegerRecipe 64)
    BuiltinTextUnconsRaw ->
      functionInfo
        TypedTextType
        TypedManagedTextRecipe
        (TypedListType (TypedTupleType [TypedCharType, TypedTextType]))
        ( TypedManagedListRecipe
            (TypedManagedProductRecipe [TypedCharRecipe, TypedManagedTextRecipe])
        )
    BuiltinTextAppend -> textBinaryInfo TypedTextType TypedManagedTextRecipe
    BuiltinTextAppendChar -> textBinaryInfo TypedCharType TypedCharRecipe
    BuiltinTextFromChars ->
      functionInfo
        (TypedListType TypedCharType)
        (TypedManagedListRecipe TypedCharRecipe)
        TypedTextType
        TypedManagedTextRecipe
    BuiltinTextConcat ->
      functionInfo
        (TypedListType TypedTextType)
        (TypedManagedListRecipe TypedManagedTextRecipe)
        TypedTextType
        TypedManagedTextRecipe
    BuiltinRenderValue ->
      functionInfo
        TypedBoolType
        TypedBoolRecipe
        TypedTextType
        TypedManagedTextRecipe
    BuiltinReadTextRaw ->
      functionInfo
        TypedTextType
        TypedManagedTextRecipe
        hostIOOutcomeType
        hostIOOutcomeRecipe
    BuiltinWriteTextRaw ->
      info
        (TypedFunctionType TypedTextType (TypedFunctionType TypedTextType hostIOOutcomeType))
        (TypedClosureRecipe [TypedManagedTextRecipe] (TypedClosureRecipe [TypedManagedTextRecipe] hostIOOutcomeRecipe))
    BuiltinReadStdinRaw ->
      functionInfo
        (TypedTupleType [])
        TypedUnitRecipe
        hostIOOutcomeType
        hostIOOutcomeRecipe
    BuiltinWriteStdoutRaw -> textToHostIOInfo
    BuiltinWriteStderrRaw -> textToHostIOInfo
    BuiltinArguments ->
      functionInfo
        (TypedTupleType [])
        TypedUnitRecipe
        (TypedListType TypedTextType)
        (TypedManagedListRecipe TypedManagedTextRecipe)
    BuiltinExit ->
      functionInfo
        TypedIntType
        (TypedSignedIntegerRecipe 64)
        (TypedTupleType [])
        TypedUnitRecipe
  where
    functionInfo argumentType argumentRecipe resultType resultRecipe =
      info
        (TypedFunctionType argumentType resultType)
        (TypedClosureRecipe [argumentRecipe] resultRecipe)
    numericConversionInfo targetType targetRecipe =
      functionInfo
        TypedIntType
        (TypedSignedIntegerRecipe 64)
        (TypedNumericType targetType)
        targetRecipe
    boolListTransformInfo =
      functionInfo
        (TypedListType TypedBoolType)
        (TypedManagedListRecipe TypedBoolRecipe)
        (TypedListType TypedBoolType)
        (TypedManagedListRecipe TypedBoolRecipe)
    charPredicateInfo =
      functionInfo TypedCharType TypedCharRecipe TypedBoolType TypedBoolRecipe
    charTransformInfo =
      functionInfo TypedCharType TypedCharRecipe TypedCharType TypedCharRecipe
    textBinaryInfo secondType secondRecipe =
      info
        (TypedFunctionType TypedTextType (TypedFunctionType secondType TypedTextType))
        (TypedClosureRecipe [TypedManagedTextRecipe] (TypedClosureRecipe [secondRecipe] TypedManagedTextRecipe))
    hostIOOutcomeType =
      TypedTupleType
        [TypedBoolType, TypedTextType, TypedTextType, TypedTextType]
    hostIOOutcomeRecipe =
      TypedManagedProductRecipe
        [TypedBoolRecipe, TypedManagedTextRecipe, TypedManagedTextRecipe, TypedManagedTextRecipe]
    textToHostIOInfo =
      functionInfo
        TypedTextType
        TypedManagedTextRecipe
        hostIOOutcomeType
        hostIOOutcomeRecipe

builtinDirectCallProgram :: TypedProgram
builtinDirectCallProgram =
  expressionFixtureProgram fixture expression
  where
    fixture = "review-builtin-direct-call-recipe"
    functionType = TypedFunctionType TypedTextType (TypedFunctionType TypedTextType TypedTextType)
    functionRecipe = TypedClosureRecipe [TypedManagedTextRecipe, TypedManagedTextRecipe] TypedManagedTextRecipe
    afterFirstInfo =
      info
        (TypedFunctionType TypedTextType TypedTextType)
        (TypedClosureRecipe [TypedManagedTextRecipe] TypedManagedTextRecipe)
    functionExpression =
      fixtureVariableExpr
        (info functionType functionRecipe)
        (TypedBuiltinName "__kernel_textAppend")
    argument literal = TypedLiteralExpr textInfo (TypedTextLiteral literal)
    expression =
      TypedApplyExpr
        textInfo
        (TypedApplyExpr afterFirstInfo functionExpression (argument "left"))
        (argument "right")

pairConstructorType :: TypedType
pairConstructorType =
  TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType pairConstructorResultType)

pairConstructorResultType :: TypedType
pairConstructorResultType =
  TypedDataType
    (resolved TypedCurrentModule TypedTypeNamespace "Pair")
    []

pairConstructorDirectRecipe :: TypedRepresentationRecipe
pairConstructorDirectRecipe =
  TypedClosureRecipe
    [TypedBoolRecipe, TypedBoolRecipe]
    pairConstructorResultRecipe

pairConstructorValueRecipe :: TypedRepresentationRecipe
pairConstructorValueRecipe =
  TypedClosureRecipe
    [TypedBoolRecipe]
    (TypedClosureRecipe [TypedBoolRecipe] pairConstructorResultRecipe)

pairConstructorResultRecipe :: TypedRepresentationRecipe
pairConstructorResultRecipe =
  TypedManagedVariantRecipe
    (resolved TypedCurrentModule TypedTypeNamespace "Pair")
    []

constructorValueRecipeProgram :: TypedProgram
constructorValueRecipeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface pairResultInfo modulePath
  where
    fixture = "review-constructor-value-recipe"
    modulePath = fixtureModulePath fixture
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Pair"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Pair"
    constructorOwner = binder modulePath [0, 0] constructorName
    declaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [ TypedConstructorDeclaration
            constructorOwner
            constructorName
            [TypedBoolType, TypedBoolType]
            [TypedBoolRecipe, TypedBoolRecipe]
        ]
    constructor recipe =
      fixtureBoundVariableExpr
        constructorOwner
        (info pairConstructorType recipe)
        constructorName
    afterFirstInfo =
      info
        (TypedFunctionType TypedBoolType pairConstructorResultType)
        (TypedClosureRecipe [TypedBoolRecipe] pairConstructorResultRecipe)
    pairResultInfo = info pairConstructorResultType pairConstructorResultRecipe
    completeCall =
      TypedApplyExpr
        pairResultInfo
        (TypedApplyExpr afterFirstInfo (constructor pairConstructorDirectRecipe) trueExpr)
        falseExpr
    statements =
      [ TypedDataStatement declaration,
        expressionStatement 1 (constructor pairConstructorDirectRecipe),
        expressionStatement 2 (constructor pairConstructorValueRecipe),
        expressionStatement 3 completeCall
      ]

builtinMapValueRecipe :: TypedRepresentationRecipe
builtinMapValueRecipe =
  TypedClosureRecipe
    [TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe]
    ( TypedClosureRecipe
        [TypedManagedListRecipe TypedBoolRecipe]
        (TypedManagedListRecipe TypedManagedTextRecipe)
    )

builtinMapValueInfo :: TypedNodeInfo
builtinMapValueInfo = info builtinMapType builtinMapValueRecipe

polymorphicBuiltinRecipeProgram :: TypedProgram
polymorphicBuiltinRecipeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface resultInfo modulePath
  where
    fixture = "review-polymorphic-builtin-value-recipe"
    modulePath = fixtureModulePath fixture
    mapperType = TypedFunctionType TypedBoolType TypedTextType
    mapperRecipe = TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe
    mapperName = resolved TypedCurrentModule TypedValueNamespace "mapper"
    mapper =
      TypedLambdaExpr
        (info mapperType mapperRecipe)
        (binder modulePath [2, 1, 0] mapperName)
        mapperName
        (TypedLiteralExpr textInfo (TypedTextLiteral "mapped"))
    intermediateInfo =
      info
        (TypedFunctionType (TypedListType TypedBoolType) (TypedListType TypedTextType))
        ( TypedClosureRecipe
            [TypedManagedListRecipe TypedBoolRecipe]
            (TypedManagedListRecipe TypedManagedTextRecipe)
        )
    argument =
      TypedListExpr
        (info (TypedListType TypedBoolType) (TypedManagedListRecipe TypedBoolRecipe))
        [trueExpr]
    resultInfo =
      info
        (TypedListType TypedTextType)
        (TypedManagedListRecipe TypedManagedTextRecipe)
    completeCall =
      TypedApplyExpr
        resultInfo
        ( TypedApplyExpr
            intermediateInfo
            (fixtureVariableExpr builtinMapDirectInfo (TypedBuiltinName "map"))
            mapper
        )
        argument
    statements =
      [ expressionStatement 1 (fixtureVariableExpr builtinMapDirectInfo (TypedBuiltinName "map")),
        expressionStatement 2 (fixtureVariableExpr builtinMapValueInfo (TypedBuiltinName "map")),
        expressionStatement 3 completeCall
      ]

builtinIntOperatorType :: TypedType
builtinIntOperatorType =
  TypedFunctionType TypedIntType (TypedFunctionType TypedIntType TypedIntType)

builtinIntOperatorDirectRecipe :: TypedRepresentationRecipe
builtinIntOperatorDirectRecipe =
  TypedClosureRecipe
    [TypedSignedIntegerRecipe 64, TypedSignedIntegerRecipe 64]
    (TypedSignedIntegerRecipe 64)

builtinIntOperatorValueRecipe :: TypedRepresentationRecipe
builtinIntOperatorValueRecipe =
  TypedClosureRecipe
    [TypedSignedIntegerRecipe 64]
    (TypedClosureRecipe [TypedSignedIntegerRecipe 64] (TypedSignedIntegerRecipe 64))

builtinOperatorValueRecipeProgram :: TypedProgram
builtinOperatorValueRecipeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface intInfo modulePath
  where
    fixture = "review-builtin-operator-value-recipe"
    modulePath = fixtureModulePath fixture
    intInfo = info TypedIntType (TypedSignedIntegerRecipe 64)
    directInfo = info builtinIntOperatorType builtinIntOperatorDirectRecipe
    valueInfo = info builtinIntOperatorType builtinIntOperatorValueRecipe
    afterFirstInfo =
      info
        (TypedFunctionType TypedIntType TypedIntType)
        (TypedClosureRecipe [TypedSignedIntegerRecipe 64] (TypedSignedIntegerRecipe 64))
    argument literal =
      TypedLiteralExpr
        intInfo
        (TypedIntegerLiteral literal)
    completeCall =
      TypedApplyExpr
        intInfo
        ( TypedApplyExpr
            afterFirstInfo
            (TypedOperatorValueExpr directInfo (TypedBuiltinOperator "+"))
            (argument "1")
        )
        (argument "2")
    statements =
      [ expressionStatement 1 (TypedOperatorValueExpr directInfo (TypedBuiltinOperator "+")),
        expressionStatement 2 (TypedOperatorValueExpr valueInfo (TypedBuiltinOperator "+")),
        expressionStatement 3 completeCall
      ]

qualifiedDirectMethodOwner :: TypedBinderId
qualifiedDirectMethodOwner =
  binder
    ["Prelude"]
    [1, 0]
    (resolved TypedCurrentModule TypedValueNamespace "render")

qualifiedDirectMethodValueProgram :: TypedProgram
qualifiedDirectMethodValueProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-qualified-direct-method-value"
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    implId = TypedImplId ["Prelude"] capabilityName [TypedTextType]
    constraint =
      TypedCapabilityConstraint
        (preludeCapability "Render")
        (Just "Render::render")
        TypedTextType
    evidenceUse =
      TypedEvidenceUse
        Nothing
        constraint
        implId
        (Just (TypedMethodId implId "render"))
    expression =
      fixtureVariableExpr
        (TypedNodeInfo boolToBoolType boolToBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBuiltinName "Render::render")

textAppendType :: TypedType
textAppendType =
  TypedFunctionType TypedTextType (TypedFunctionType TypedTextType TypedTextType)

textAppendDirectRecipe :: TypedRepresentationRecipe
textAppendDirectRecipe =
  TypedClosureRecipe
    [TypedManagedTextRecipe, TypedManagedTextRecipe]
    TypedManagedTextRecipe

textAppendValueRecipe :: TypedRepresentationRecipe
textAppendValueRecipe =
  TypedClosureRecipe
    [TypedManagedTextRecipe]
    (TypedClosureRecipe [TypedManagedTextRecipe] TypedManagedTextRecipe)

directCallableResultValueProgram :: TypedProgram
directCallableResultValueProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-direct-callable-result-value"
    modulePath = fixtureModulePath fixture
    functionName = resolved TypedCurrentModule TypedValueNamespace "choose"
    functionOwner = binder modulePath [0] functionName
    parameterName = resolved TypedCurrentModule TypedValueNamespace "ignored"
    functionType = TypedFunctionType TypedBoolType textAppendType
    functionRecipe = TypedClosureRecipe [TypedBoolRecipe] textAppendDirectRecipe
    functionInfo = info functionType functionRecipe
    functionScheme =
      fixtureScheme
        functionOwner
        []
        []
        []
        functionType
        functionRecipe
    expression =
      TypedLambdaExpr
        functionInfo
        (binder modulePath [0, 0] parameterName)
        parameterName
        ( fixtureVariableExpr
            (info textAppendType textAppendDirectRecipe)
            (TypedBuiltinName "__kernel_textAppend")
        )
    statements =
      [ TypedLetStatement functionOwner functionName span1 functionScheme expression,
        expressionStatement 2 trueExpr
      ]

implMethodCallableShapeOwner :: TypedBinderId
implMethodCallableShapeOwner =
  binder
    (fixtureModulePath "review-impl-method-callable-shape")
    [0, 0]
    (resolved TypedCurrentModule TypedValueNamespace "choose")

implMethodCallableShapeProgram :: TypedProgram
implMethodCallableShapeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-impl-method-callable-shape"
    modulePath = fixtureModulePath fixture
    parameter = TypedTypeParameterId 0
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Chooser"
    methodName = resolved TypedCurrentModule TypedValueNamespace "choose"
    methodType = TypedFunctionType TypedBoolType boolToBoolType
    methodRecipe = TypedClosureRecipe [TypedBoolRecipe] boolToBoolRecipe
    methodScheme =
      fixtureScheme
        implMethodCallableShapeOwner
        []
        []
        []
        methodType
        methodRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    firstParameter = resolved TypedCurrentModule TypedValueNamespace "first"
    secondParameter = resolved TypedCurrentModule TypedValueNamespace "second"
    methodExpression =
      TypedLambdaExpr
        (info methodType methodRecipe)
        (binder modulePath [1, 0, 0] firstParameter)
        firstParameter
        ( TypedLambdaExpr
            boolToBoolInfo
            (binder modulePath [1, 0, 0, 0] secondParameter)
            secondParameter
            trueExpr
        )
    methodDefinition =
      TypedMethodDefinition
        (TypedMethodId implId "choose")
        (binder modulePath [1, 0] methodName)
        methodName
        span1
        methodExpression
    statements =
      [ TypedClassStatement classDeclaration,
        TypedImplStatement (TypedImplDeclaration span1 implId [methodDefinition]),
        expressionStatement 3 trueExpr
      ]

publishedImplWithoutCapabilityMetadataId :: TypedImplId
publishedImplWithoutCapabilityMetadataId =
  TypedImplId
    (fixtureModulePath "review-published-impl-capability-metadata")
    (resolved TypedCurrentModule TypedCapabilityNamespace "Published")
    [TypedBoolType]

publishedImplWithoutCapabilityMetadataProgram :: TypedProgram
publishedImplWithoutCapabilityMetadataProgram =
  singleModuleProgram fixture relativeSource [] statements interface unitInfo modulePath
  where
    fixture = "review-published-impl-capability-metadata"
    modulePath = fixtureModulePath fixture
    capability =
      TypedClassDeclaration
        span1
        (resolved TypedCurrentModule TypedCapabilityNamespace "Published")
        [TypedTypeParameterId 0]
        []
    statements =
      [ TypedClassStatement capability,
        TypedImplStatement (TypedImplDeclaration span1 publishedImplWithoutCapabilityMetadataId [])
      ]
    interface =
      TypedModuleInterface
        []
        []
        []
        [TypedImplInterface publishedImplWithoutCapabilityMetadataId]

deferredCandidateSelectedImpl :: TypedImplId
deferredCandidateSelectedImpl =
  fixtureRenderImpl (fixtureModulePath "review-deferred-candidate-selection")

deferredCandidateSelectionProgram :: TypedProgram
deferredCandidateSelectionProgram =
  qualifiedMapDispatchProgram
    fixture
    [ TypedEvidenceCandidates
        fixtureRenderConstraint
        [fixtureRenderCandidate (fixtureRenderImpl ["Prelude"])]
    ]
    [ TypedSelectedEvidence
        ( TypedEvidenceUse
            Nothing
            fixtureRenderConstraint
            deferredCandidateSelectedImpl
            (Just (TypedMethodId deferredCandidateSelectedImpl "map"))
        )
    ]
  where
    fixture = "review-deferred-candidate-selection"

selectedEvidenceProgressionOriginalImpl :: TypedImplId
selectedEvidenceProgressionOriginalImpl =
  fixtureRenderImpl ["Prelude"]

selectedEvidenceProgressionProgram :: TypedProgram
selectedEvidenceProgressionProgram =
  qualifiedMapDispatchProgram
    fixture
    [selected selectedEvidenceProgressionOriginalImpl]
    [selected (fixtureRenderImpl (fixtureModulePath fixture))]
  where
    fixture = "review-selected-evidence-progression"
    selected implId =
      TypedSelectedEvidence
        ( TypedEvidenceUse
            Nothing
            fixtureRenderConstraint
            implId
            (Just (TypedMethodId implId "map"))
        )

mismatchedExplicitInstantiationOwner :: TypedBinderId
mismatchedExplicitInstantiationOwner =
  binder
    (fixtureModulePath "review-mismatched-explicit-instantiation")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "identity")

mismatchedExplicitInstantiationProgram :: TypedProgram
mismatchedExplicitInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface outerInfo modulePath
  where
    fixture = "review-mismatched-explicit-instantiation"
    modulePath = fixtureModulePath fixture
    name = resolved TypedCurrentModule TypedValueNamespace "identity"
    owner = mismatchedExplicitInstantiationOwner
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    scheme =
      fixtureClosureScheme
        owner
        [parameter]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    instantiate typeArgument =
      TypedInstantiation
        owner
        [TypedTypeArgument parameter typeArgument]
        (Just span1)
    outerInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        [instantiate TypedBoolType]
        []
    calleeInfo =
      TypedNodeInfo
        (TypedFunctionType TypedCharType TypedCharType)
        (TypedClosureRecipe [TypedCharRecipe] TypedCharRecipe)
        [instantiate TypedCharType]
        []
    expression =
      TypedTypeApplicationExpr
        outerInfo
        (fixtureVariableExpr calleeInfo name)
        span1
        TypedBoolType
    statements =
      [ TypedLetStatement
          owner
          name
          span1
          scheme
          (polymorphicIdentityExpression modulePath [0] parameter),
        expressionStatement 1 expression
      ]

nestedPathProgram :: TypedProgram
nestedPathProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-path"
    modulePath = (fixtureModulePath fixture)
    nestedName = TypedUnresolvedSourceName "nested"
    block =
      TypedBlockExpr
        (info TypedBoolType (TypedSignedIntegerRecipe 64))
        [expressionStatement 2 (fixtureVariableExpr boolInfo nestedName)]

nestedPathFailures :: [TypedCoreValidationFailure]
nestedPathFailures =
  [ TypedCoreValidationFailure
      (TypedExpressionPath (fixtureModulePath "review-nested-path") [0] [0])
      TypedTypeRepresentationMismatch
      (TypedRecipeDetail TypedBoolRecipe (TypedSignedIntegerRecipe 64)),
    TypedCoreValidationFailure
      (TypedExpressionPath (fixtureModulePath "review-nested-path") [0, 0, 0] [0])
      TypedUnresolvedName
      (TypedNameDetail (TypedUnresolvedSourceName "nested"))
  ]

nestedDeclarationProgram :: TypedProgram
nestedDeclarationProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-declaration"
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Flag"
    constructorBinder = binder modulePath [0, 0] constructorName
    declaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [TypedConstructorDeclaration constructorBinder constructorName [TypedBoolType] [TypedSignedIntegerRecipe 64]]
    block = TypedBlockExpr boolInfo [TypedDataStatement declaration]

nestedDeclarationFailures :: [TypedCoreValidationFailure]
nestedDeclarationFailures =
  [ TypedCoreValidationFailure
      (TypedExpressionPath (fixtureModulePath "review-nested-declaration") [0] [0])
      TypedBlockResultMismatch
      TypedNoValidationDetail,
    TypedCoreValidationFailure
      (TypedStatementPath (fixtureModulePath "review-nested-declaration") [0, 0, 0])
      TypedBlockResultMismatch
      (TypedTextDetail "data declaration")
  ]

nestedDuplicateBinderProgram :: TypedProgram
nestedDuplicateBinderProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-duplicate-binder"
    modulePath = (fixtureModulePath fixture)
    name = resolved TypedCurrentModule TypedValueNamespace "duplicate"
    duplicateBinder = binder modulePath [0, 0] name
    scheme = monoScheme duplicateBinder
    block =
      TypedBlockExpr
        boolInfo
        [ TypedLetStatement duplicateBinder name span1 scheme trueExpr,
          TypedLetStatement duplicateBinder name span1 scheme trueExpr,
          expressionStatement 3 trueExpr
        ]

nestedDuplicateBinderFailures :: [TypedCoreValidationFailure]
nestedDuplicateBinderFailures =
  [ TypedCoreValidationFailure
      (TypedStatementPath (fixtureModulePath "review-nested-duplicate-binder") [0, 0, 1])
      TypedDuplicateBinder
      (TypedBinderDetail (binder (fixtureModulePath "review-nested-duplicate-binder") [0, 0] (resolved TypedCurrentModule TypedValueNamespace "duplicate")))
  ]

guardedCasePathProgram :: TypedProgram
guardedCasePathProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface boolInfo modulePath
  where
    fixture = "review-guarded-case-path"
    modulePath = (fixtureModulePath fixture)
    unresolved name = fixtureVariableExpr boolInfo (TypedUnresolvedSourceName name)
    expression =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [ TypedCaseArm (TypedWildcardPattern boolInfo) (Just (unresolved "guard")) (unresolved "first-result"),
          TypedCaseArm (TypedWildcardPattern boolInfo) Nothing (unresolved "second-result")
        ]

generalizedLetScopeProgram :: TypedProgram
generalizedLetScopeProgram =
  singleModuleProgram fixture relativeSource [TypedModuleExport TypedValueNamespace "identity"] [statement] interface boolInfo modulePath
  where
    fixture = "review-generalized-let-scope"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "identity"
    valueBinder = binder modulePath [0] valueName
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentBinder = binder modulePath [0, 0] argumentName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    functionInfo = info functionType functionRecipe
    scheme = fixtureScheme valueBinder [parameterId] [] [] functionType functionRecipe
    expression = TypedLambdaExpr functionInfo argumentBinder argumentName (fixtureBoundVariableExpr argumentBinder (info parameterType parameterRecipe) argumentName)
    statement = TypedLetStatement valueBinder valueName span1 scheme expression
    interface = TypedModuleInterface [TypedValueInterface valueName scheme] [] [] []

importedInstantiationProgram :: TypedProgram
importedInstantiationProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "Identity")
    entryPath = (fixtureModulePath "review-imported-instantiation")
    localName = resolved TypedCurrentModule TypedValueNamespace "identity"
    importedName = resolved (TypedImportedModule libraryPath) TypedValueNamespace "identity"
    owner = binder libraryPath [0] localName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    scheme =
      fixtureClosureScheme
        owner
        [parameterId]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/Identity.jz")
        []
        [TypedModuleExport TypedValueNamespace "identity"]
        (TypedModuleInterface [TypedValueInterface localName scheme] [] [] [])
        [TypedLetStatement owner localName span1 scheme (polymorphicIdentityExpression libraryPath [0] parameterId)]
        boolInfo
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] Nothing
    instantiatedType = TypedFunctionType TypedBoolType TypedBoolType
    instantiatedRecipe = TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe
    instantiatedInfo = TypedNodeInfo instantiatedType instantiatedRecipe [instantiation] []
    expression = fixtureBoundVariableExpr owner instantiatedInfo importedName
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["identity"])]
        []
        emptyInterface
        [expressionStatement 1 expression]
        instantiatedInfo

invisibleSiblingImplId :: TypedImplId
invisibleSiblingImplId =
  TypedImplId ["Hidden", "Evidence"] (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal") [TypedBoolType]

retainedPreludeEqualClass :: TypedClassDeclaration
retainedPreludeEqualClass = fixtureEqualClass TypedAmbientPrelude

fixtureEqualClass :: TypedNameOrigin -> TypedClassDeclaration
fixtureEqualClass origin =
  TypedClassDeclaration
    span1
    (resolved origin TypedCapabilityNamespace "Equal")
    [TypedTypeParameterId 0]
    [ TypedMethodSignature
        (resolved origin TypedValueNamespace "equal")
        span1
        (monoScheme equalOwner),
      TypedMethodSignature
        (resolved origin TypedValueNamespace "other")
        span1
        (fixtureScheme otherOwner [] [] [] boolToBoolType boolToBoolRecipe)
    ]
  where
    equalOwner =
      binder
        ["Prelude"]
        [0, 0]
        (resolved TypedCurrentModule TypedValueNamespace "equal")
    otherOwner =
      binder
        ["Prelude"]
        [0, 1]
        (resolved TypedCurrentModule TypedValueNamespace "other")

invisibleSiblingImplProgram :: TypedProgram
invisibleSiblingImplProgram = TypedProgram (Just fixturePrelude) [hiddenModule, entryModule] entryPath
  where
    fixture = "review-invisible-sibling-impl"
    hiddenPath = ["Hidden", "Evidence"]
    entryPath = (fixtureModulePath fixture)
    hiddenDeclaration =
      TypedImplDeclaration
        span1
        invisibleSiblingImplId
        [ fixtureImplMethod hiddenPath [0, 0] invisibleSiblingImplId "equal",
          fixtureImplMethod hiddenPath [0, 1] invisibleSiblingImplId "other"
        ]
    hiddenModule =
      typedModule
        hiddenPath
        (TypedSourcePath "src/Hidden/Evidence.jz")
        []
        []
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface retainedPreludeEqualClass]
            [TypedImplInterface invisibleSiblingImplId]
        )
        [TypedImplStatement hiddenDeclaration]
        boolInfo
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    valueBinder = binder entryPath [0] valueName
    scheme = monoScheme valueBinder
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    evidence = TypedEvidenceUse Nothing constraint invisibleSiblingImplId Nothing
    expression = fixtureBoundVariableExpr valueBinder (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidence]) valueName
    entryModule =
      typedModule
        entryPath
        relativeSource
        []
        []
        emptyInterface
        [TypedLetStatement valueBinder valueName span1 scheme trueExpr, expressionStatement 1 expression]
        boolInfo

selectedEvidenceTargetProgram :: TypedProgram
selectedEvidenceTargetProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-selected-evidence-target"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedCharType]
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    valueBinder = binder modulePath [1] valueName
    scheme = monoScheme valueBinder
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    evidence = TypedEvidenceUse Nothing constraint implId Nothing
    expression = fixtureBoundVariableExpr valueBinder (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidence]) valueName
    statements =
      [ TypedImplStatement
          ( TypedImplDeclaration
              span1
              implId
              [ fixtureImplMethod modulePath [0, 0] implId "equal",
                fixtureImplMethod modulePath [0, 1] implId "other"
              ]
          ),
        TypedLetStatement valueBinder valueName span1 scheme trueExpr,
        expressionStatement 1 expression
      ]

invisibleVariableName :: TypedCoreName
invisibleVariableName = resolved TypedCurrentModule TypedValueNamespace "missing"

invisibleVariableProgram :: TypedProgram
invisibleVariableProgram =
  expressionFixtureProgram "review-invisible-variable" (fixtureVariableExpr boolInfo invisibleVariableName)

selectedMethodContractProgram :: TypedProgram
selectedMethodContractProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-selected-method-contract"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    valueName = resolved TypedCurrentModule TypedValueNamespace "equal"
    valueBinder = binder modulePath [1] valueName
    scheme = monoScheme valueBinder
    constraint = TypedCapabilityConstraint capabilityName (Just "Equal.equal") TypedBoolType
    withoutMethod = TypedEvidenceUse Nothing constraint implId Nothing
    wrongMethod = TypedEvidenceUse Nothing constraint implId (Just (TypedMethodId implId "other"))
    selected evidence = fixtureBoundVariableExpr valueBinder (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidence]) valueName
    statements =
      [ TypedImplStatement
          ( TypedImplDeclaration
              span1
              implId
              [ fixtureImplMethod modulePath [0, 0] implId "equal",
                fixtureImplMethod modulePath [0, 1] implId "other"
              ]
          ),
        TypedLetStatement valueBinder valueName span1 scheme trueExpr,
        expressionStatement 1 (selected withoutMethod),
        expressionStatement 2 (selected wrongMethod)
      ]

enclosingOtherImplId :: TypedImplId
enclosingOtherImplId =
  TypedImplId (fixtureModulePath "review-enclosing-impl-method") (resolved TypedAmbientPrelude TypedCapabilityNamespace "Render") [TypedCharType]

enclosingImplMethodProgram :: TypedProgram
enclosingImplMethodProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] [TypedImplStatement declaration] emptyInterface boolInfo modulePath)
  where
    fixture = "review-enclosing-impl-method"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    implId = TypedImplId modulePath capabilityName [TypedTextType]
    methodName = resolved TypedCurrentModule TypedValueNamespace "render"
    methodBinder = binder modulePath [0, 0] methodName
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    methodExpression = TypedLambdaExpr boolToBoolInfo (binder modulePath [0, 0, 0] argumentName) argumentName trueExpr
    method = TypedMethodDefinition (TypedMethodId enclosingOtherImplId "render") methodBinder methodName span1 methodExpression
    declaration =
      TypedImplDeclaration
        span1
        implId
        [method, fixtureImplMethod modulePath [0, 1] implId "map"]

bindingValueProgram :: TypedProgram
bindingValueProgram =
  singleModuleProgram fixture relativeSource [] [TypedLetStatement valueBinder valueName span1 (monoScheme valueBinder) value] emptyInterface boolInfo modulePath
  where
    fixture = "review-binding-value"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    valueBinder = binder modulePath [0] valueName
    value = literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "wrong")

lambdaResultProgram :: TypedProgram
lambdaResultProgram = expressionFixtureProgram fixture expression
  where
    fixture = "review-lambda-result"
    modulePath = (fixtureModulePath fixture)
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    expression =
      TypedLambdaExpr
        boolToBoolInfo
        (binder modulePath [0, 0] argumentName)
        argumentName
        (literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "wrong"))

literalTypeProgram :: TypedProgram
literalTypeProgram = expressionFixtureProgram "review-literal-type" (TypedLiteralExpr boolInfo (TypedTextLiteral "wrong"))

collectionShapeProgram :: TypedProgram
collectionShapeProgram =
  expressionFixtureProgram
    "review-collection-shape"
    (TypedListExpr boolListInfo [literalExpr TypedCharType TypedCharRecipe (TypedCharacterLiteral 'x')])

dataTypeArityProgram :: TypedProgram
dataTypeArityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface dataInfo modulePath
  where
    fixture = "review-data-type-arity"
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    declaration =
      dataDeclarationWithNullaryConstructor
        modulePath
        [0, 0]
        dataName
        [TypedTypeParameterId 0]
    dataInfo = info (TypedDataType dataName []) (TypedManagedVariantRecipe dataName [])
    statements =
      [ TypedDataStatement declaration,
        expressionStatement 1 (TypedBlockExpr dataInfo [])
      ]

tuplePatternShapeProgram :: TypedProgram
tuplePatternShapeProgram = expressionFixtureProgram fixture expression
  where
    fixture = "review-tuple-pattern-shape"
    expression =
      TypedPatternCaseExpr
        boolInfo
        (TypedTupleExpr pairInfo [trueExpr, falseExpr])
        [TypedCaseArm (TypedTuplePattern pairInfo [TypedWildcardPattern boolInfo]) Nothing trueExpr]

moduleResultProgram :: TypedProgram
moduleResultProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 terminal] emptyInterface boolInfo (fixtureModulePath fixture)
  where
    fixture = "review-module-result"
    terminal = literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "result")

stagedModuleResultProgram :: TypedProgram
stagedModuleResultProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    [expressionStatement 1 terminal]
    emptyInterface
    flattenedInfo
    modulePath
  where
    fixture = "review-staged-module-result"
    modulePath = fixtureModulePath fixture
    flattenedInfo = info callableResultType flattenedCallableResultRecipe
    terminal = stagedCallableResultExpression modulePath [0]

stagedBlockResultProgram :: TypedProgram
stagedBlockResultProgram =
  expressionFixtureProgram
    fixture
    ( TypedBlockExpr
        (info callableResultType flattenedCallableResultRecipe)
        [expressionStatement 2 (stagedCallableResultExpression modulePath [0, 0, 0])]
    )
  where
    fixture = "review-staged-block-result"
    modulePath = fixtureModulePath fixture

callableResultType :: TypedType
callableResultType = TypedFunctionType TypedBoolType (TypedFunctionType TypedCharType TypedTextType)

flattenedCallableResultRecipe :: TypedRepresentationRecipe
flattenedCallableResultRecipe =
  TypedClosureRecipe [TypedBoolRecipe, TypedCharRecipe] TypedManagedTextRecipe

stagedCallableResultRecipe :: TypedRepresentationRecipe
stagedCallableResultRecipe =
  TypedClosureRecipe
    [TypedBoolRecipe]
    (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe)

stagedCallableResultExpression :: [Text] -> [Int] -> TypedExpr
stagedCallableResultExpression modulePath lexicalPath =
  TypedLambdaExpr
    (info callableResultType stagedCallableResultRecipe)
    outerBinder
    outerName
    ( TypedLambdaExpr
        (info (TypedFunctionType TypedCharType TypedTextType) (TypedClosureRecipe [TypedCharRecipe] TypedManagedTextRecipe))
        innerBinder
        innerName
        (TypedLiteralExpr textInfo (TypedTextLiteral "result"))
    )
  where
    outerName = fixtureValueName "outer"
    outerBinder = binder modulePath lexicalPath outerName
    innerName = fixtureValueName "inner"
    innerBinder = binder modulePath (lexicalPath <> [0]) innerName

missingSchemeDataName :: TypedCoreName
missingSchemeDataName = resolved TypedCurrentModule TypedTypeNamespace "Missing"

schemeDataTypeProgram :: TypedProgram
schemeDataTypeProgram =
  singleModuleProgram fixture relativeSource [] [TypedSignatureStatement valueBinder valueName span1 scheme] emptyInterface boolInfo modulePath
  where
    fixture = "review-scheme-data-type"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    valueBinder = binder modulePath [0] valueName
    resultType = TypedDataType missingSchemeDataName []
    scheme = fixtureScheme valueBinder [] [] [] resultType (TypedManagedVariantRecipe missingSchemeDataName [])

driveAbsoluteProgram :: TypedProgram
driveAbsoluteProgram =
  singleModuleProgram fixture (TypedSourcePath "C:/Fixture/Main.jz") [] [] emptyInterface boolInfo (fixtureModulePath fixture)
  where
    fixture = "review-drive-absolute"

forwardVisibilityFailure :: Text -> [Int] -> TypedCoreName -> TypedCoreValidationFailure
forwardVisibilityFailure fixture expressionPath name =
  forwardVisibilityFailureAt fixture 1 expressionPath name

forwardVisibilityFailureAt :: Text -> Int -> [Int] -> TypedCoreName -> TypedCoreValidationFailure
forwardVisibilityFailureAt fixture statementIndex expressionPath name =
  TypedCoreValidationFailure
    (TypedExpressionPath (fixtureModulePath fixture) [statementIndex] expressionPath)
    TypedInvisibleName
    (TypedNameDetail name)

validationKinds :: TypedProgram -> [TypedCoreValidationKind]
validationKinds program =
  [kind | TypedCoreValidationFailure _ kind _ <- validateTypedProgram program]

emptyImportSelectorProgram :: TypedProgram
emptyImportSelectorProgram =
  importSelectorShapeProgram "review-empty-import-selector" []

duplicateImportSelectorProgram :: TypedProgram
duplicateImportSelectorProgram =
  importSelectorShapeProgram "review-duplicate-import-selector" ["item", "item"]

aliasAndSelectorImportProgram :: TypedProgram
aliasAndSelectorImportProgram =
  importSelectorProgram
    "review-alias-and-selector-import"
    (Just "Library")
    ["item"]

importSelectorShapeProgram :: Text -> [Text] -> TypedProgram
importSelectorShapeProgram fixture selectedNames =
  importSelectorProgram fixture Nothing selectedNames

importSelectorProgram :: Text -> Maybe Text -> [Text] -> TypedProgram
importSelectorProgram fixture alias selectedNames =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath fixture)
    entryPath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    valueOwner = binder libraryPath [0] valueName
    valueScheme = monoScheme valueOwner
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath ("src/Library/" <> fixture <> ".jz"))
        []
        [TypedModuleExport TypedValueNamespace "item"]
        (TypedModuleInterface [TypedValueInterface valueName valueScheme] [] [] [])
        [TypedLetStatement valueOwner valueName span1 valueScheme trueExpr]
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath alias (Just selectedNames)]
        []
        emptyInterface
        []
        unitInfo

sameNamedCapabilityProviderPath :: [Text]
sameNamedCapabilityProviderPath =
  fixtureLibraryPath "SameNamedCapabilityProvider"

sameNamedImportedCapabilityName :: TypedCoreName
sameNamedImportedCapabilityName =
  resolved
    (TypedImportedModule sameNamedCapabilityProviderPath)
    TypedCapabilityNamespace
    "Shared"

sameNamedImportedImplId :: TypedImplId
sameNamedImportedImplId =
  TypedImplId
    sameNamedCapabilityProviderPath
    sameNamedImportedCapabilityName
    [TypedBoolType]

sameNamedCapabilityProviderModule :: TypedModule
sameNamedCapabilityProviderModule =
  sameNamedCapabilityProviderModuleAt sameNamedCapabilityProviderPath

sameNamedCapabilityProviderModuleAt :: [Text] -> TypedModule
sameNamedCapabilityProviderModuleAt providerPath =
  typedModule
    providerPath
    (TypedSourcePath "src/Library/SameNamedCapabilityProvider.jz")
    []
    [TypedModuleExport TypedValueNamespace "source"]
    ( TypedModuleInterface
        [TypedValueInterface sourceName sourceScheme]
        []
        [TypedClassInterface capability]
        [TypedImplInterface localImplId]
    )
    [ TypedClassStatement capability,
      TypedImplStatement (TypedImplDeclaration span1 localImplId []),
      TypedLetStatement sourceOwner sourceName span1 sourceScheme trueExpr
    ]
    unitInfo
  where
    parameter = TypedTypeParameterId 0
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Shared"
    capability =
      TypedClassDeclaration span1 capabilityName [parameter] []
    localImplId =
      TypedImplId
        providerPath
        capabilityName
        [TypedBoolType]
    sourceName =
      resolved TypedCurrentModule TypedValueNamespace "source"
    sourceOwner =
      binder providerPath [2] sourceName
    sourceScheme =
      fixtureScheme
        sourceOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint capabilityName Nothing TypedBoolType)
        ]
        []
        TypedBoolType
        TypedBoolRecipe

sameNamedVisibleCapabilityProviderModuleAt :: [Text] -> TypedModule
sameNamedVisibleCapabilityProviderModuleAt providerPath =
  typedModule
    providerPath
    (TypedSourcePath "src/Library/SameNamedVisibleCapabilityProvider.jz")
    []
    [TypedModuleExport TypedCapabilityNamespace "Shared"]
    (TypedModuleInterface [] [] [TypedClassInterface capability] [])
    [TypedClassStatement capability]
    unitInfo
  where
    capability =
      TypedClassDeclaration
        span1
        (resolved TypedCurrentModule TypedCapabilityNamespace "Shared")
        [TypedTypeParameterId 0]
        []

sameNamedCapabilityFacadePath :: [Text]
sameNamedCapabilityFacadePath =
  fixtureLibraryPath "SameNamedCapabilityFacadeMissing"

sameNamedCapabilityDependencyProgram :: TypedProgram
sameNamedCapabilityDependencyProgram =
  TypedProgram
    Nothing
    [ sameNamedVisibleCapabilityProviderModuleAt sameNamedCapabilityProviderPath,
      secondProviderModule,
      facadeModule
    ]
    sameNamedCapabilityFacadePath
  where
    secondProviderPath =
      fixtureLibraryPath "SameNamedCapabilityProviderTwo"
    secondProviderModule =
      sameNamedVisibleCapabilityProviderModuleAt secondProviderPath
    secondImportedCapabilityName =
      resolved
        (TypedImportedModule secondProviderPath)
        TypedCapabilityNamespace
        "Shared"
    retainedWrongCapability =
      TypedClassDeclaration
        span1
        secondImportedCapabilityName
        [TypedTypeParameterId 0]
        []
    publishedName =
      resolved TypedCurrentModule TypedValueNamespace "published"
    publishedOwner =
      binder sameNamedCapabilityFacadePath [0] publishedName
    publishedScheme =
      fixtureScheme
        publishedOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            ( TypedCapabilityConstraint
                sameNamedImportedCapabilityName
                Nothing
                TypedBoolType
            )
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    facadeModule =
      typedModule
        sameNamedCapabilityFacadePath
        (TypedSourcePath "src/Library/SameNamedCapabilityFacadeMissing.jz")
        [ TypedResolvedImport
            span1
            sameNamedCapabilityProviderPath
            (Just "First")
            Nothing,
          TypedResolvedImport
            span1
            secondProviderPath
            (Just "Second")
            Nothing
        ]
        [TypedModuleExport TypedValueNamespace "published"]
        ( TypedModuleInterface
            [TypedValueInterface publishedName publishedScheme]
            []
            [TypedClassInterface retainedWrongCapability]
            []
        )
        [ TypedLetStatement
            publishedOwner
            publishedName
            span1
            publishedScheme
            trueExpr
        ]
        unitInfo

sameNamedRetainedCapabilityProgram :: TypedProgram
sameNamedRetainedCapabilityProgram =
  TypedProgram
    Nothing
    [sameNamedCapabilityProviderModule, facadeModule, entryModule]
    entryPath
  where
    facadePath =
      fixtureLibraryPath "SameNamedCapabilityFacade"
    entryPath =
      fixtureModulePath "review-same-named-retained-capability"
    parameter = TypedTypeParameterId 0
    localCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Shared"
    localCapability =
      TypedClassDeclaration span1 localCapabilityName [parameter] []
    retainedCapability =
      TypedClassDeclaration span1 sameNamedImportedCapabilityName [parameter] []
    constraint =
      TypedCapabilityConstraint
        sameNamedImportedCapabilityName
        Nothing
        TypedBoolType
    facadeModule =
      typedModule
        facadePath
        (TypedSourcePath "src/Library/SameNamedCapabilityFacade.jz")
        [ TypedResolvedImport
            span1
            sameNamedCapabilityProviderPath
            Nothing
            (Just ["source"])
        ]
        [TypedModuleExport TypedCapabilityNamespace "Shared"]
        ( TypedModuleInterface
            []
            []
            [ TypedClassInterface localCapability,
              TypedClassInterface retainedCapability
            ]
            [TypedImplInterface sameNamedImportedImplId]
        )
        [TypedClassStatement localCapability]
        unitInfo
    evidenceUse =
      TypedEvidenceUse
        Nothing
        constraint
        sameNamedImportedImplId
        Nothing
    expression =
      TypedLiteralExpr
        ( TypedNodeInfo
            TypedBoolType
            TypedBoolRecipe
            []
            [TypedSelectedEvidence evidenceUse]
        )
        (TypedBooleanLiteral True)
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 facadePath Nothing (Just ["Shared"])]
        []
        emptyInterface
        [expressionStatement 1 expression]
        boolInfo

distinctClassMethodProgram :: TypedProgram
distinctClassMethodProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-distinct-class-methods"
    modulePath = (fixtureModulePath fixture)
    methodName = resolved TypedCurrentModule TypedValueNamespace "render"
    classDeclaration statementIndex classIdentifier =
      let methodOwner = binder modulePath [statementIndex, 0] methodName
          methodScheme = monoScheme methodOwner
       in TypedClassDeclaration
            span1
            (resolved TypedCurrentModule TypedCapabilityNamespace classIdentifier)
            [TypedTypeParameterId 0]
            [TypedMethodSignature methodName span1 methodScheme]
    statements =
      [ TypedClassStatement (classDeclaration 0 "Render"),
        TypedClassStatement (classDeclaration 1 "Debug")
      ]

duplicateEvidenceConstraintProgram :: TypedProgram
duplicateEvidenceConstraintProgram =
  withFixturePrelude (signatureProgram fixture valueOwner valueName valueScheme)
  where
    fixture = "review-duplicate-evidence-constraint"
    valueName = fixtureValueName "item"
    valueOwner = fixtureBinder fixture 0 valueName
    constraint =
      TypedCapabilityConstraint
        (preludeCapability "Equal")
        Nothing
        TypedBoolType
    valueScheme =
      fixtureScheme
        valueOwner
        []
        [ TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint,
          TypedEvidenceParameter (TypedEvidenceParameterId 1) constraint
        ]
        []
        TypedBoolType
        TypedBoolRecipe

singletonTupleTypeProgram :: TypedProgram
singletonTupleTypeProgram =
  expressionFixtureProgram
    "review-singleton-tuple-type"
    (TypedTupleExpr singletonInfo [trueExpr])
  where
    singletonInfo =
      info
        (TypedTupleType [TypedBoolType])
        (TypedManagedProductRecipe [TypedBoolRecipe])

preludeAmbientDependencyName :: TypedCoreName
preludeAmbientDependencyName =
  resolved TypedAmbientPrelude TypedTypeNamespace "Payload"

preludeAmbientDataDependencyProgram :: TypedProgram
preludeAmbientDataDependencyProgram =
  TypedProgram (Just preludeModule) [entryModule] entryPath
  where
    fixture = "review-prelude-ambient-data-dependency"
    entryPath = (fixtureModulePath fixture)
    dataDeclaration =
      dataDeclarationWithNullaryConstructor
        ["Prelude"]
        [0, 0]
        preludeAmbientDependencyName
        []
    className =
      resolved TypedCurrentModule TypedCapabilityNamespace "ProvidesPayload"
    methodName =
      resolved TypedCurrentModule TypedValueNamespace "payload"
    methodOwner = binder ["Prelude"] [1, 0] methodName
    methodScheme =
      fixtureScheme
        methodOwner
        []
        []
        []
        (TypedDataType preludeAmbientDependencyName [])
        (TypedManagedVariantRecipe preludeAmbientDependencyName [])
    classDeclaration =
      TypedClassDeclaration
        span1
        className
        [TypedTypeParameterId 0]
        [TypedMethodSignature methodName span1 methodScheme]
    preludeModule =
      typedModule
        ["Prelude"]
        (TypedSourcePath "src/Prelude.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "ProvidesPayload"]
        (TypedModuleInterface [] [] [TypedClassInterface classDeclaration] [])
        [ TypedDataStatement dataDeclaration,
          TypedClassStatement classDeclaration
        ]
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        []
        []
        emptyInterface
        []
        unitInfo

duplicateInterfaceModulePath :: [Text]
duplicateInterfaceModulePath =
  (fixtureModulePath "review-duplicate-interface-entries")

duplicateInterfaceValueName :: TypedCoreName
duplicateInterfaceValueName =
  resolved TypedCurrentModule TypedValueNamespace "published"

duplicateInterfaceDataName :: TypedCoreName
duplicateInterfaceDataName =
  resolved TypedCurrentModule TypedTypeNamespace "Flag"

duplicateInterfaceClassName :: TypedCoreName
duplicateInterfaceClassName =
  resolved TypedCurrentModule TypedCapabilityNamespace "Render"

duplicateInterfaceImplId :: TypedImplId
duplicateInterfaceImplId =
  TypedImplId
    duplicateInterfaceModulePath
    duplicateInterfaceClassName
    [TypedBoolType]

duplicateModuleInterfaceEntriesProgram :: TypedProgram
duplicateModuleInterfaceEntriesProgram =
  TypedProgram Nothing [moduleValue] duplicateInterfaceModulePath
  where
    valueOwner =
      binder duplicateInterfaceModulePath [0] duplicateInterfaceValueName
    valueScheme = monoScheme valueOwner
    valueInterface =
      TypedValueInterface duplicateInterfaceValueName valueScheme
    dataDeclaration =
      dataDeclarationWithNullaryConstructor
        duplicateInterfaceModulePath
        [1, 0]
        duplicateInterfaceDataName
        []
    dataInterface = TypedDataInterface dataDeclaration
    classDeclaration =
      TypedClassDeclaration
        span1
        duplicateInterfaceClassName
        [TypedTypeParameterId 0]
        []
    classInterface = TypedClassInterface classDeclaration
    implInterface = TypedImplInterface duplicateInterfaceImplId
    moduleValue =
      typedModule
        duplicateInterfaceModulePath
        relativeSource
        []
        [TypedModuleExport TypedValueNamespace "published"]
        ( TypedModuleInterface
            [valueInterface, valueInterface]
            [dataInterface, dataInterface]
            [classInterface, classInterface]
            [implInterface, implInterface]
        )
        [ TypedLetStatement valueOwner duplicateInterfaceValueName span1 valueScheme trueExpr,
          TypedDataStatement dataDeclaration,
          TypedClassStatement classDeclaration,
          TypedImplStatement
            (TypedImplDeclaration span1 duplicateInterfaceImplId [])
        ]
        unitInfo

cyclicImportFirstPath :: [Text]
cyclicImportFirstPath = ["Cycle", "First"]

cyclicImportSecondPath :: [Text]
cyclicImportSecondPath = ["Cycle", "Second"]

cyclicImportProgram :: TypedProgram
cyclicImportProgram =
  TypedProgram
    Nothing
    [ moduleWithImport cyclicImportFirstPath cyclicImportSecondPath,
      moduleWithImport cyclicImportSecondPath cyclicImportFirstPath
    ]
    cyclicImportFirstPath
  where
    moduleWithImport modulePath importPath =
      typedModule
        modulePath
        (TypedSourcePath ("src/" <> Text.intercalate "/" modulePath <> ".jz"))
        [TypedResolvedImport span1 importPath Nothing Nothing]
        []
        emptyInterface
        []
        unitInfo

bareSignatureValueName :: TypedCoreName
bareSignatureValueName =
  resolved TypedCurrentModule TypedValueNamespace "declaredOnly"

bareSignatureVisibilityProgram :: TypedProgram
bareSignatureVisibilityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-bare-signature-visibility"
    modulePath = (fixtureModulePath fixture)
    owner = binder modulePath [0] bareSignatureValueName
    statements =
      [ TypedSignatureStatement owner bareSignatureValueName span1 (monoScheme owner),
        expressionStatement 1 (fixtureVariableExpr boolInfo bareSignatureValueName)
      ]

activeRebindingExportName :: TypedCoreName
activeRebindingExportName =
  resolved TypedCurrentModule TypedValueNamespace "item"

activeRebindingExportProgram :: TypedProgram
activeRebindingExportProgram =
  singleModuleProgram fixture relativeSource exports statements interface unitInfo modulePath
  where
    fixture = "review-active-rebinding-export"
    modulePath = (fixtureModulePath fixture)
    firstOwner = binder modulePath [0] activeRebindingExportName
    secondOwner = binder modulePath [1] activeRebindingExportName
    firstScheme = monoScheme firstOwner
    secondScheme =
      fixtureScheme secondOwner [] [] [] TypedTextType TypedManagedTextRecipe
    exports = [TypedModuleExport TypedValueNamespace "item"]
    statements =
      [ TypedLetStatement firstOwner activeRebindingExportName span1 firstScheme trueExpr,
        TypedLetStatement
          secondOwner
          activeRebindingExportName
          span1
          secondScheme
          (TypedLiteralExpr textInfo (TypedTextLiteral "latest"))
      ]
    interface =
      TypedModuleInterface
        [TypedValueInterface activeRebindingExportName firstScheme]
        []
        []
        []

constructorInstantiationProgram :: TypedProgram
constructorInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface constructorInfo modulePath
  where
    fixture = "review-constructor-instantiation"
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Option"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    parameterId = TypedTypeParameterId 0
    constructorOwner = binder modulePath [0, 0] constructorName
    declaration =
      TypedDataDeclaration
        span1
        dataName
        [parameterId]
        [ TypedConstructorDeclaration
            constructorOwner
            constructorName
            [TypedTypeParameterType parameterId]
            [TypedRepresentationParameterRecipe parameterId]
        ]
    instantiation =
      TypedInstantiation
        constructorOwner
        [TypedTypeArgument parameterId TypedBoolType]
        Nothing
    constructorInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (TypedDataType dataName [TypedBoolType]))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedManagedVariantRecipe dataName [TypedBoolType]))
        [instantiation]
        []
    statements =
      [ TypedDataStatement declaration,
        expressionStatement 1 (fixtureBoundVariableExpr constructorOwner constructorInfo constructorName)
      ]

missingConstructorInstantiationOwner :: TypedBinderId
missingConstructorInstantiationOwner =
  binder
    (fixtureModulePath "review-missing-constructor-instantiation")
    [0, 0]
    (resolved TypedCurrentModule TypedConstructorNamespace "Some")

missingConstructorInstantiationProgram :: TypedProgram
missingConstructorInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface constructorInfo modulePath
  where
    fixture = "review-missing-constructor-instantiation"
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Option"
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    parameterId = TypedTypeParameterId 0
    declaration =
      TypedDataDeclaration
        span1
        dataName
        [parameterId]
        [ TypedConstructorDeclaration
            missingConstructorInstantiationOwner
            constructorName
            [TypedTypeParameterType parameterId]
            [TypedRepresentationParameterRecipe parameterId]
        ]
    constructorInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (TypedDataType dataName [TypedBoolType]))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedManagedVariantRecipe dataName [TypedBoolType]))
        []
        []
    statements =
      [ TypedDataStatement declaration,
        expressionStatement 1 (fixtureBoundVariableExpr missingConstructorInstantiationOwner constructorInfo constructorName)
      ]

retainedCapabilityEvidenceProgram :: TypedProgram
retainedCapabilityEvidenceProgram =
  TypedProgram Nothing [providerModule, facadeModule, entryModule] entryPath
  where
    providerPath = (fixtureLibraryPath "RetainedCapabilityProvider")
    facadePath = (fixtureLibraryPath "RetainedCapabilityFacade")
    entryPath = (fixtureModulePath "review-retained-capability-evidence")
    parameter = TypedTypeParameterId 0
    localCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "ForeignEq"
    importedCapabilityName =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        "ForeignEq"
    localCapability =
      TypedClassDeclaration span1 localCapabilityName [parameter] []
    retainedCapability =
      TypedClassDeclaration span1 importedCapabilityName [parameter] []
    localImplId =
      TypedImplId providerPath localCapabilityName [TypedBoolType]
    retainedImplId =
      TypedImplId providerPath importedCapabilityName [TypedBoolType]
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/RetainedCapabilityProvider.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "ForeignEq"]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface localCapability]
            [TypedImplInterface localImplId]
        )
        [ TypedClassStatement localCapability,
          TypedImplStatement (TypedImplDeclaration span1 localImplId [])
        ]
        unitInfo
    publishedName =
      resolved TypedCurrentModule TypedValueNamespace "published"
    publishedOwner = binder facadePath [0] publishedName
    constraint = TypedCapabilityConstraint importedCapabilityName Nothing TypedBoolType
    publishedScheme =
      fixtureScheme
        publishedOwner
        []
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    facadeModule =
      typedModule
        facadePath
        (TypedSourcePath "src/Library/RetainedCapabilityFacade.jz")
        [TypedResolvedImport span1 providerPath Nothing (Just ["ForeignEq"])]
        [TypedModuleExport TypedValueNamespace "published"]
        ( TypedModuleInterface
            [TypedValueInterface publishedName publishedScheme]
            []
            [TypedClassInterface retainedCapability]
            [TypedImplInterface retainedImplId]
        )
        [TypedLetStatement publishedOwner publishedName span1 publishedScheme trueExpr]
        unitInfo
    importedPublishedName =
      resolved
        (TypedImportedModule facadePath)
        TypedValueNamespace
        "published"
    instantiation = TypedInstantiation publishedOwner [] Nothing
    evidenceUse =
      TypedEvidenceUse
        ( Just
            ( TypedEvidenceParameterRef
                publishedOwner
                (TypedEvidenceParameterId 0)
            )
        )
        constraint
        retainedImplId
        Nothing
    entryInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [instantiation]
        [TypedSelectedEvidence evidenceUse]
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 facadePath Nothing (Just ["published"])]
        []
        emptyInterface
        [expressionStatement 1 (fixtureVariableExpr entryInfo importedPublishedName)]
        entryInfo

retainedCapabilityWrongImplName :: TypedCoreName
retainedCapabilityWrongImplName =
  resolved TypedCurrentModule TypedCapabilityNamespace "Visible"

retainedCapabilityWrongImplProgram :: TypedProgram
retainedCapabilityWrongImplProgram =
  TypedProgram Nothing [providerModule, entryModule] entryPath
  where
    providerPath = fixtureLibraryPath "RetainedCapabilityWrongImpl"
    entryPath = fixtureModulePath "review-retained-capability-wrong-impl"
    parameter = TypedTypeParameterId 0
    providerCapability =
      resolved TypedCurrentModule TypedCapabilityNamespace "Hidden"
    providerMethod =
      resolved TypedCurrentModule TypedValueNamespace "render"
    providerMethodOwner = binder providerPath [0, 0] providerMethod
    providerMethodScheme =
      fixtureScheme
        providerMethodOwner
        []
        []
        []
        (TypedFunctionType (TypedTypeParameterType parameter) TypedTextType)
        (TypedClosureRecipe [TypedRepresentationParameterRecipe parameter] TypedManagedTextRecipe)
    providerClass =
      TypedClassDeclaration
        span1
        providerCapability
        [parameter]
        [TypedMethodSignature providerMethod span1 providerMethodScheme]
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/RetainedCapabilityWrongImpl.jz")
        []
        [ TypedModuleExport TypedCapabilityNamespace "Hidden",
          TypedModuleExport TypedValueNamespace "render"
        ]
        (TypedModuleInterface [] [] [TypedClassInterface providerClass] [])
        [TypedClassStatement providerClass]
        unitInfo
    importedCapability =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        "Hidden"
    visibleClass =
      TypedClassDeclaration
        span1
        retainedCapabilityWrongImplName
        [parameter]
        []
    wrongImplId =
      TypedImplId entryPath retainedCapabilityWrongImplName [TypedBoolType]
    localName =
      resolved TypedCurrentModule TypedValueNamespace "local"
    localOwner = binder entryPath [2] localName
    evidenceParameter = TypedEvidenceParameterId 0
    constraint =
      TypedCapabilityConstraint importedCapability Nothing TypedBoolType
    localScheme =
      fixtureScheme
        localOwner
        []
        [TypedEvidenceParameter evidenceParameter constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    evidenceUse =
      TypedEvidenceUse
        (Just (TypedEvidenceParameterRef localOwner evidenceParameter))
        constraint
        wrongImplId
        Nothing
    entryInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [TypedInstantiation localOwner [] Nothing]
        [TypedSelectedEvidence evidenceUse]
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 providerPath Nothing (Just ["render"])]
        []
        emptyInterface
        [ TypedClassStatement visibleClass,
          TypedImplStatement (TypedImplDeclaration span1 wrongImplId []),
          TypedLetStatement localOwner localName span1 localScheme trueExpr,
          expressionStatement 4 (fixtureVariableExpr entryInfo localName)
        ]
        entryInfo

missingPublishedImplId :: TypedImplId
missingPublishedImplId =
  TypedImplId
    (fixtureModulePath "review-missing-published-impl")
    (resolved TypedCurrentModule TypedCapabilityNamespace "Comparable")
    [TypedBoolType]

missingPublishedImplProgram :: TypedProgram
missingPublishedImplProgram =
  singleModuleProgram fixture relativeSource exports statements interface unitInfo modulePath
  where
    fixture = "review-missing-published-impl"
    modulePath = (fixtureModulePath fixture)
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Comparable"
    capability =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    exports = [TypedModuleExport TypedCapabilityNamespace "Comparable"]
    statements =
      [ TypedClassStatement capability,
        TypedImplStatement (TypedImplDeclaration span1 missingPublishedImplId [])
      ]
    interface =
      TypedModuleInterface [] [] [TypedClassInterface capability] []

expandingRecursiveEqualityProgram :: TypedProgram
expandingRecursiveEqualityProgram =
  recursiveEqualityProgram "review-expanding-recursive-equality" False

recursiveEqualityCallableType :: TypedType
recursiveEqualityCallableType =
  TypedDataType
    (resolved TypedCurrentModule TypedTypeNamespace "Nest")
    [TypedBoolType]

recursiveEqualityCallableFieldProgram :: TypedProgram
recursiveEqualityCallableFieldProgram =
  recursiveEqualityProgram "review-recursive-equality-callable-field" True

recursiveEqualityProgram :: Text -> Bool -> TypedProgram
recursiveEqualityProgram fixture includeCallableField =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Nest"
    recursiveConstructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Nest"
    callableConstructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Callable"
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    recursiveArgument = TypedListType parameterType
    recursiveField = TypedDataType dataName [recursiveArgument]
    recursiveConstructor =
      TypedConstructorDeclaration
        (binder modulePath [0, 0] recursiveConstructorName)
        recursiveConstructorName
        [recursiveField]
        [TypedManagedVariantRecipe dataName [recursiveArgument]]
    callableConstructor =
      TypedConstructorDeclaration
        (binder modulePath [0, 1] callableConstructorName)
        callableConstructorName
        [boolToBoolType]
        [boolToBoolRecipe]
    declaration =
      TypedDataDeclaration
        span1
        dataName
        [parameter]
        ( recursiveConstructor
            : [callableConstructor | includeCallableField]
        )
    valueName = resolved TypedCurrentModule TypedValueNamespace "equality"
    valueOwner = binder modulePath [1] valueName
    targetType = TypedDataType dataName [TypedBoolType]
    scheme =
      fixtureScheme
        valueOwner
        []
        []
        [TypedStrictEqualityPrimitiveConstraint targetType]
        TypedBoolType
        TypedBoolRecipe
    statements =
      [ TypedDataStatement declaration,
        TypedLetStatement valueOwner valueName span1 scheme trueExpr
      ]

orphanSignatureName :: TypedCoreName
orphanSignatureName =
  resolved TypedCurrentModule TypedValueNamespace "orphan"

orphanSignatureProgram :: TypedProgram
orphanSignatureProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-orphan-signature"
    modulePath = (fixtureModulePath fixture)
    owner = binder modulePath [0] orphanSignatureName
    statements =
      [TypedSignatureStatement owner orphanSignatureName span1 (monoScheme owner)]

recursiveEqualityNestedCallableType :: TypedType
recursiveEqualityNestedCallableType =
  TypedDataType
    (resolved TypedCurrentModule TypedTypeNamespace "Nest")
    [TypedBoolType]

recursiveEqualityNestedCallableProgram :: TypedProgram
recursiveEqualityNestedCallableProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-recursive-equality-nested-callable"
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Nest"
    stepName = resolved TypedCurrentModule TypedConstructorNamespace "Step"
    baseName = resolved TypedCurrentModule TypedConstructorNamespace "Base"
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    callableParameter = TypedFunctionType parameterType parameterType
    recursiveField = TypedDataType dataName [callableParameter]
    declaration =
      TypedDataDeclaration
        span1
        dataName
        [parameter]
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] stepName)
            stepName
            [recursiveField]
            [TypedManagedVariantRecipe dataName [callableParameter]],
          TypedConstructorDeclaration
            (binder modulePath [0, 1] baseName)
            baseName
            [parameterType]
            [TypedRepresentationParameterRecipe parameter]
        ]
    valueName = resolved TypedCurrentModule TypedValueNamespace "equality"
    valueOwner = binder modulePath [1] valueName
    scheme =
      fixtureScheme
        valueOwner
        []
        []
        [TypedStrictEqualityPrimitiveConstraint recursiveEqualityNestedCallableType]
        TypedBoolType
        TypedBoolRecipe
    statements =
      [ TypedDataStatement declaration,
        TypedLetStatement valueOwner valueName span1 scheme trueExpr
      ]

importedCurrentOriginName :: TypedCoreName
importedCurrentOriginName =
  resolved
    (TypedImportedModule (fixtureModulePath "review-imported-current-origin"))
    TypedValueNamespace
    "item"

importedCurrentOriginProgram :: TypedProgram
importedCurrentOriginProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-imported-current-origin"
    modulePath = (fixtureModulePath fixture)
    localName = resolved TypedCurrentModule TypedValueNamespace "item"
    owner = binder modulePath [0] localName
    statements =
      [ TypedLetStatement owner localName span1 (monoScheme owner) trueExpr,
        expressionStatement
          2
          (fixtureBoundVariableExpr owner boolInfo importedCurrentOriginName)
      ]

retainedCapabilityExportProgram :: TypedProgram
retainedCapabilityExportProgram =
  case retainedCapabilityEvidenceProgram of
    TypedProgram prelude modules entryPath ->
      TypedProgram prelude (map addCapabilityExport modules) entryPath
  where
    facadePath = (fixtureLibraryPath "RetainedCapabilityFacade")
    addCapabilityExport moduleValue@(TypedModule modulePath sourcePath imports exports interface recursiveGroups statements moduleInfo)
      | modulePath == facadePath =
          TypedModule
            modulePath
            sourcePath
            imports
            (TypedModuleExport TypedCapabilityNamespace "ForeignEq" : exports)
            interface
            recursiveGroups
            statements
            moduleInfo
      | otherwise = moduleValue

importAliasCollisionProgram :: TypedProgram
importAliasCollisionProgram =
  TypedProgram Nothing [leftModule, rightModule, entryModule] entryPath
  where
    leftPath = ["Alias", "Left"]
    rightPath = ["Alias", "Right"]
    entryPath = (fixtureModulePath "review-import-alias-collision")
    dependency path sourcePath =
      typedModule path (TypedSourcePath sourcePath) [] [] emptyInterface [] unitInfo
    leftModule = dependency leftPath "src/Alias/Left.jz"
    rightModule = dependency rightPath "src/Alias/Right.jz"
    entryModule =
      typedModule
        entryPath
        relativeSource
        [ TypedResolvedImport span1 leftPath (Just "Ops") Nothing,
          TypedResolvedImport span1 rightPath (Just "Ops") Nothing
        ]
        []
        emptyInterface
        []
        unitInfo

implBeforeClassCapabilityName :: TypedCoreName
implBeforeClassCapabilityName =
  resolved TypedCurrentModule TypedCapabilityNamespace "Deferred"

implBeforeClassProgram :: TypedProgram
implBeforeClassProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-impl-before-class"
    modulePath = (fixtureModulePath fixture)
    implId =
      TypedImplId modulePath implBeforeClassCapabilityName [TypedBoolType]
    declaration =
      TypedClassDeclaration
        span1
        implBeforeClassCapabilityName
        [TypedTypeParameterId 0]
        []
    statements =
      [ TypedImplStatement (TypedImplDeclaration span1 implId []),
        TypedClassStatement declaration
      ]

evidenceCapabilityWrongName :: TypedCoreName
evidenceCapabilityWrongName =
  resolved
    (TypedImportedModule ["Evidence", "Right"])
    TypedCapabilityNamespace
    "Shared"

evidenceCapabilityOriginProgram :: TypedProgram
evidenceCapabilityOriginProgram =
  TypedProgram Nothing [leftModule, rightModule, entryModule] entryPath
  where
    leftPath = ["Evidence", "Left"]
    rightPath = ["Evidence", "Right"]
    entryPath = (fixtureModulePath "review-evidence-capability-origin")
    provider modulePath sourcePath publishedIdentifier =
      typedModule
        modulePath
        (TypedSourcePath sourcePath)
        []
        [TypedModuleExport TypedValueNamespace publishedIdentifier]
        ( TypedModuleInterface
            [TypedValueInterface publishedName publishedScheme]
            []
            [TypedClassInterface classDeclaration]
            [TypedImplInterface localImplId]
        )
        [ TypedClassStatement classDeclaration,
          TypedImplStatement
            (TypedImplDeclaration span1 localImplId []),
          TypedLetStatement
            publishedOwner
            publishedName
            span1
            publishedScheme
            trueExpr
        ]
        unitInfo
      where
        capabilityName =
          resolved TypedCurrentModule TypedCapabilityNamespace "Shared"
        constraint =
          TypedCapabilityConstraint capabilityName Nothing TypedBoolType
        classDeclaration =
          TypedClassDeclaration
            span1
            capabilityName
            [TypedTypeParameterId 0]
            []
        localImplId =
          TypedImplId modulePath capabilityName [TypedBoolType]
        publishedName =
          resolved TypedCurrentModule TypedValueNamespace publishedIdentifier
        publishedOwner = binder modulePath [2] publishedName
        publishedScheme =
          fixtureScheme
            publishedOwner
            []
            [ TypedEvidenceParameter
                (TypedEvidenceParameterId 0)
                constraint
            ]
            []
            TypedBoolType
            TypedBoolRecipe
    leftModule = provider leftPath "src/Evidence/Left.jz" "left"
    rightModule = provider rightPath "src/Evidence/Right.jz" "right"
    leftName =
      resolved (TypedImportedModule leftPath) TypedValueNamespace "left"
    leftCapabilityName =
      resolved (TypedImportedModule leftPath) TypedCapabilityNamespace "Shared"
    leftConstraint =
      TypedCapabilityConstraint leftCapabilityName Nothing TypedBoolType
    leftOwner = binder leftPath [2] (resolved TypedCurrentModule TypedValueNamespace "left")
    wrongImplId =
      TypedImplId rightPath evidenceCapabilityWrongName [TypedBoolType]
    evidenceUse =
      TypedEvidenceUse
        ( Just
            ( TypedEvidenceParameterRef
                leftOwner
                (TypedEvidenceParameterId 0)
            )
        )
        leftConstraint
        wrongImplId
        Nothing
    entryInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [TypedInstantiation leftOwner [] Nothing]
        [TypedSelectedEvidence evidenceUse]
    entryModule =
      typedModule
        entryPath
        relativeSource
        [ TypedResolvedImport span1 leftPath Nothing (Just ["left"]),
          TypedResolvedImport span1 rightPath Nothing (Just ["right"])
        ]
        []
        emptyInterface
        [expressionStatement 1 (fixtureVariableExpr entryInfo leftName)]
        entryInfo

malformedGeneratedNamesProgram :: TypedProgram
malformedGeneratedNamesProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-malformed-generated-names"
    modulePath = (fixtureModulePath fixture)
    invalidLambdaName =
      TypedGeneratedName (TypedLambdaPatternArgument 0)
    invalidLambda =
      TypedLambdaExpr
        boolToBoolInfo
        (binder modulePath [0, 0] invalidLambdaName)
        invalidLambdaName
        trueExpr
    emptyOperatorName = TypedGeneratedName (TypedOperatorBinding "")
    malformedOperatorName =
      TypedGeneratedName (TypedOperatorBinding "operator:%2B")
    unencodedOperatorName =
      TypedGeneratedName (TypedOperatorBinding "$operator:garbage")
    invalidHexOperatorName =
      TypedGeneratedName (TypedOperatorBinding "$operator:%GG")
    builtinOperatorName =
      TypedGeneratedName (TypedOperatorBinding "$operator:%2B")
    reservedOperatorName =
      TypedGeneratedName (TypedOperatorBinding "$operator:%2D%3E")
    emptyOperatorOwner = binder modulePath [1] emptyOperatorName
    malformedOperatorOwner = binder modulePath [2] malformedOperatorName
    unencodedOperatorOwner = binder modulePath [3] unencodedOperatorName
    invalidHexOperatorOwner = binder modulePath [4] invalidHexOperatorName
    builtinOperatorOwner = binder modulePath [5] builtinOperatorName
    reservedOperatorOwner = binder modulePath [6] reservedOperatorName
    statements =
      [ expressionStatement 1 invalidLambda,
        TypedLetStatement
          emptyOperatorOwner
          emptyOperatorName
          span1
          (monoScheme emptyOperatorOwner)
          trueExpr,
        TypedLetStatement
          malformedOperatorOwner
          malformedOperatorName
          span1
          (monoScheme malformedOperatorOwner)
          trueExpr,
        TypedLetStatement
          unencodedOperatorOwner
          unencodedOperatorName
          span1
          (monoScheme unencodedOperatorOwner)
          trueExpr,
        TypedLetStatement
          invalidHexOperatorOwner
          invalidHexOperatorName
          span1
          (monoScheme invalidHexOperatorOwner)
          trueExpr,
        TypedLetStatement
          builtinOperatorOwner
          builtinOperatorName
          span1
          (monoScheme builtinOperatorOwner)
          trueExpr,
        TypedLetStatement
          reservedOperatorOwner
          reservedOperatorName
          span1
          (monoScheme reservedOperatorOwner)
          trueExpr
      ]

regularPreludeModuleProgram :: TypedProgram
regularPreludeModuleProgram =
  TypedProgram
    Nothing
    [ typedModule
        ["Prelude"]
        (TypedSourcePath "src/Prelude.jz")
        []
        []
        emptyInterface
        []
        unitInfo
    ]
    ["Prelude"]

retainedClassMethodExportProgram :: TypedProgram
retainedClassMethodExportProgram =
  TypedProgram Nothing [providerModule, facadeModule] facadePath
  where
    providerPath = (fixtureLibraryPath "RetainedMethodProvider")
    facadePath = (fixtureLibraryPath "RetainedMethodFacade")
    parameter = TypedTypeParameterId 0
    localClassName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Display"
    localMethodName =
      resolved TypedCurrentModule TypedValueNamespace "display"
    methodOwner = binder providerPath [0, 0] localMethodName
    methodScheme =
      fixtureScheme
        methodOwner
        []
        []
        []
        (TypedFunctionType (TypedTypeParameterType parameter) TypedTextType)
        ( TypedClosureRecipe
            [TypedRepresentationParameterRecipe parameter]
            TypedManagedTextRecipe
        )
    localClass =
      TypedClassDeclaration
        span1
        localClassName
        [parameter]
        [TypedMethodSignature localMethodName span1 methodScheme]
    publishedName =
      resolved TypedCurrentModule TypedValueNamespace "published"
    publishedOwner = binder providerPath [1] publishedName
    publishedScheme =
      fixtureScheme
        publishedOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint localClassName Nothing TypedBoolType)
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/RetainedMethodProvider.jz")
        []
        [TypedModuleExport TypedValueNamespace "published"]
        ( TypedModuleInterface
            [TypedValueInterface publishedName publishedScheme]
            []
            [TypedClassInterface localClass]
            []
        )
        [ TypedClassStatement localClass,
          TypedLetStatement publishedOwner publishedName span1 publishedScheme trueExpr
        ]
        unitInfo
    retainedClassName =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        "Display"
    retainedMethodName =
      resolved
        (TypedImportedModule providerPath)
        TypedValueNamespace
        "display"
    retainedClass =
      TypedClassDeclaration
        span1
        retainedClassName
        [parameter]
        [TypedMethodSignature retainedMethodName span1 methodScheme]
    facadeModule =
      typedModule
        facadePath
        (TypedSourcePath "src/Library/RetainedMethodFacade.jz")
        [TypedResolvedImport span1 providerPath Nothing (Just ["published"])]
        [TypedModuleExport TypedValueNamespace "display"]
        (TypedModuleInterface [] [] [TypedClassInterface retainedClass] [])
        []
        unitInfo

malformedWhitespaceName :: TypedCoreName
malformedWhitespaceName =
  resolved TypedCurrentModule TypedValueNamespace "bad name"

malformedReservedName :: TypedCoreName
malformedReservedName =
  resolved TypedCurrentModule TypedValueNamespace "if"

malformedQualifiedName :: TypedCoreName
malformedQualifiedName =
  resolved TypedCurrentModule TypedValueNamespace "Other::render"

malformedResolvedIdentifiersProgram :: TypedProgram
malformedResolvedIdentifiersProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-malformed-resolved-identifiers"
    modulePath = (fixtureModulePath fixture)
    binding statementIndex name =
      let owner = binder modulePath [statementIndex] name
       in TypedLetStatement owner name span1 (monoScheme owner) trueExpr
    statements =
      [ binding 0 malformedWhitespaceName,
        binding 1 malformedReservedName,
        binding 2 malformedQualifiedName
      ]

normalizedPreludeAmbientImpl :: TypedImplId
normalizedPreludeAmbientImpl =
  TypedImplId
    ["Prelude"]
    (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal")
    [TypedBoolType]

normalizedPreludeImplDuplicatesProgram :: TypedProgram
normalizedPreludeImplDuplicatesProgram =
  TypedProgram (Just preludeModule) [] ["Prelude"]
  where
    preludePath = ["Prelude"]
    localCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    localClass =
      TypedClassDeclaration
        span1
        localCapabilityName
        [TypedTypeParameterId 0]
        []
    localImpl =
      TypedImplId preludePath localCapabilityName [TypedBoolType]
    preludeModule =
      typedModule
        preludePath
        (TypedSourcePath "src/Prelude.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Equal"]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface localClass]
            [ TypedImplInterface localImpl,
              TypedImplInterface normalizedPreludeAmbientImpl
            ]
        )
        [ TypedClassStatement localClass,
          TypedImplStatement (TypedImplDeclaration span1 localImpl []),
          TypedImplStatement
            (TypedImplDeclaration span1 normalizedPreludeAmbientImpl [])
        ]
        unitInfo

malformedImportAliasProgram :: TypedProgram
malformedImportAliasProgram =
  TypedProgram Nothing [dependencyModule, entryModule] entryPath
  where
    dependencyPath = ["Alias", "Dependency"]
    entryPath = (fixtureModulePath "review-malformed-import-alias")
    dependencyModule =
      typedModule
        dependencyPath
        (TypedSourcePath "src/Alias/Dependency.jz")
        []
        []
        emptyInterface
        []
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 dependencyPath (Just "True") Nothing]
        []
        emptyInterface
        []
        unitInfo

duplicateModuleExportsProgram :: TypedProgram
duplicateModuleExportsProgram =
  singleModuleProgram
    fixture
    relativeSource
    [ duplicateExport,
      duplicateExport
    ]
    [TypedLetStatement owner name span1 scheme trueExpr]
    (TypedModuleInterface [TypedValueInterface name scheme] [] [] [])
    unitInfo
    modulePath
  where
    fixture = "review-duplicate-module-exports"
    modulePath = (fixtureModulePath fixture)
    name = resolved TypedCurrentModule TypedValueNamespace "answer"
    owner = binder modulePath [0] name
    scheme = monoScheme owner
    duplicateExport = TypedModuleExport TypedValueNamespace "answer"

invalidSpan :: TypedSpan
invalidSpan = TypedSpan 0 (-1)

invalidImportSpanProgram :: TypedProgram
invalidImportSpanProgram =
  TypedProgram Nothing [dependencyModule, entryModule] entryPath
  where
    dependencyPath = ["Span", "Dependency"]
    entryPath = (fixtureModulePath "review-invalid-import-span")
    dependencyModule =
      typedModule
        dependencyPath
        (TypedSourcePath "src/Span/Dependency.jz")
        []
        []
        emptyInterface
        []
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport invalidSpan dependencyPath Nothing Nothing]
        []
        emptyInterface
        []
        unitInfo

invalidStatementSpansProgram :: TypedProgram
invalidStatementSpansProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-invalid-statement-spans"
    modulePath = (fixtureModulePath fixture)
    name = resolved TypedCurrentModule TypedValueNamespace "answer"
    signatureOwner = binder modulePath [0] name
    bindingOwner = binder modulePath [1] name
    signatureScheme = monoScheme signatureOwner
    bindingScheme = monoScheme bindingOwner
    statements =
      [ TypedSignatureStatement signatureOwner name invalidSpan signatureScheme,
        TypedLetStatement bindingOwner name invalidSpan bindingScheme trueExpr,
        TypedExpressionStatement invalidSpan trueExpr
      ]

invalidDeclarationSpansProgram :: TypedProgram
invalidDeclarationSpansProgram =
  TypedProgram (Just invalidPrelude) [] ["Prelude"]
  where
    invalidPrelude =
      case fixturePrelude of
        TypedModule modulePath sourcePath imports exports interface recursiveGroups statements moduleInfo ->
          TypedModule
            modulePath
            sourcePath
            imports
            exports
            (invalidateInterface interface)
            recursiveGroups
            (map invalidateStatement statements)
            moduleInfo
    invalidateInterface (TypedModuleInterface values datas classes impls) =
      TypedModuleInterface
        values
        datas
        [TypedClassInterface (invalidateClass declaration) | TypedClassInterface declaration <- classes]
        impls
    invalidateStatement statement =
      case statement of
        TypedClassStatement declaration ->
          TypedClassStatement (invalidateClass declaration)
        TypedImplStatement declaration ->
          TypedImplStatement (invalidateImpl declaration)
        other -> other
    invalidateClass (TypedClassDeclaration _ name parameters methods) =
      TypedClassDeclaration
        invalidSpan
        name
        parameters
        [TypedMethodSignature methodName invalidSpan scheme | TypedMethodSignature methodName _ scheme <- methods]
    invalidateImpl (TypedImplDeclaration _ implId methods) =
      TypedImplDeclaration
        invalidSpan
        implId
        [ TypedMethodDefinition methodId owner name invalidSpan expression
        | TypedMethodDefinition methodId owner name _ expression <- methods
        ]

invalidExpressionSpansProgram :: TypedProgram
invalidExpressionSpansProgram =
  instantiationProgram "review-invalid-expression-spans" (Just invalidSpan)

resolvedModuleOrderImporterPath :: [Text]
resolvedModuleOrderImporterPath =
  (fixtureModulePath "review-resolved-module-order")

resolvedModuleOrderProgram :: TypedProgram
resolvedModuleOrderProgram =
  TypedProgram
    Nothing
    [ typedModule
        resolvedModuleOrderImporterPath
        relativeSource
        [TypedResolvedImport span1 dependencyPath Nothing Nothing]
        []
        emptyInterface
        []
        unitInfo,
      typedModule
        dependencyPath
        (TypedSourcePath "src/Dependency/Library.jz")
        []
        []
        emptyInterface
        []
        unitInfo
    ]
    resolvedModuleOrderImporterPath
  where
    dependencyPath = ["Dependency", "Library"]

emptyResolvedIdentifierName :: TypedCoreName
emptyResolvedIdentifierName =
  resolved TypedCurrentModule TypedValueNamespace ""

emptyResolvedIdentifierProgram :: TypedProgram
emptyResolvedIdentifierProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-empty-resolved-identifier"
    modulePath = (fixtureModulePath fixture)
    owner = binder modulePath [0] emptyResolvedIdentifierName
    statements =
      [ TypedLetStatement owner emptyResolvedIdentifierName span1 (monoScheme owner) trueExpr,
        expressionStatement 1 (fixtureBoundVariableExpr owner boolInfo emptyResolvedIdentifierName)
      ]

explicitSpanOnVariableOwner :: TypedBinderId
explicitSpanOnVariableOwner =
  binder
    (fixtureModulePath "review-explicit-span-on-variable")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "identity")

explicitSpanOnVariableProgram :: TypedProgram
explicitSpanOnVariableProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface instantiatedInfo modulePath
  where
    fixture = "review-explicit-span-on-variable"
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    valueName = resolved TypedCurrentModule TypedValueNamespace "identity"
    scheme =
      fixtureClosureScheme
        explicitSpanOnVariableOwner
        [parameter]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    instantiation =
      TypedInstantiation
        explicitSpanOnVariableOwner
        [TypedTypeArgument parameter TypedBoolType]
        (Just span1)
    instantiatedInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        [instantiation]
        []
    statements =
      [ TypedLetStatement
          explicitSpanOnVariableOwner
          valueName
          span1
          scheme
          (polymorphicIdentityExpression modulePath [0] parameter),
        expressionStatement 1 (fixtureVariableExpr instantiatedInfo valueName)
      ]

singleEvidenceCandidateProgram :: TypedProgram
singleEvidenceCandidateProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-single-evidence-candidate"
    capabilityName =
      resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint =
      TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    implId =
      TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    candidate =
      TypedEvidenceCandidate implId Nothing
    expression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedEvidenceCandidates constraint [candidate]])
        (TypedBooleanLiteral True)

emptyModulePathProgram :: TypedProgram
emptyModulePathProgram =
  TypedProgram
    Nothing
    [ typedModule
        []
        relativeSource
        []
        []
        emptyInterface
        []
        unitInfo
    ]
    []

wrongPreludeSlotProgram :: TypedProgram
wrongPreludeSlotProgram =
  TypedProgram
    ( Just
        ( typedModule
            (fixtureLibraryPath "WrongPrelude")
            (TypedSourcePath "src/Library/WrongPrelude.jz")
            []
            []
            emptyInterface
            []
            unitInfo
        )
    )
    [ typedModule
        entryPath
        relativeSource
        []
        []
        emptyInterface
        []
        unitInfo
    ]
    entryPath
  where
    entryPath = (fixtureModulePath "review-wrong-prelude-slot")

signatureBindingMismatchProgram :: TypedProgram
signatureBindingMismatchProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-signature-binding-mismatch"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "annotated"
    signatureOwner = binder modulePath [0] valueName
    bindingOwner = binder modulePath [1] valueName
    signatureScheme = monoScheme signatureOwner
    bindingScheme =
      fixtureScheme bindingOwner [] [] [] TypedTextType TypedManagedTextRecipe
    statements =
      [ TypedSignatureStatement signatureOwner valueName span1 signatureScheme,
        TypedLetStatement
          bindingOwner
          valueName
          span1
          bindingScheme
          (TypedLiteralExpr textInfo (TypedTextLiteral "value"))
      ]

signatureBindingShapeMismatchProgram :: TypedProgram
signatureBindingShapeMismatchProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-signature-binding-shape-mismatch"
    modulePath = fixtureModulePath fixture
    valueName = resolved TypedCurrentModule TypedValueNamespace "annotated"
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    signatureOwner = binder modulePath [0] valueName
    bindingOwner = shapeBindingOwner
    argumentOwner = binder modulePath [1, 0] argumentName
    signatureScheme =
      TypedScheme signatureOwner [] [] [] boolToBoolType boolToBoolRecipe (Just TypedDirectCallableShape)
    bindingScheme =
      TypedScheme bindingOwner [] [] [] boolToBoolType boolToBoolRecipe (Just TypedClosureCallableShape)
    statements =
      [ TypedSignatureStatement signatureOwner valueName span1 signatureScheme,
        TypedLetStatement
          bindingOwner
          valueName
          span1
          bindingScheme
          ( TypedLambdaExpr
              boolToBoolInfo
              argumentOwner
              argumentName
              (fixtureBoundVariableExpr argumentOwner boolInfo argumentName)
          )
      ]

shapeBindingOwner :: TypedBinderId
shapeBindingOwner =
  binder
    (fixtureModulePath "review-signature-binding-shape-mismatch")
    [1]
    (resolved TypedCurrentModule TypedValueNamespace "annotated")

qualifiedMethodTypeApplicationProgram :: TypedProgram
qualifiedMethodTypeApplicationProgram =
  TypedProgram (Just preludeModule) [entryModule] entryPath
  where
    preludePath = ["Prelude"]
    entryPath = (fixtureModulePath "review-qualified-method-type-application")
    parameter = TypedTypeParameterId 0
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Printable"
    methodName =
      resolved TypedCurrentModule TypedValueNamespace "print!"
    methodOwner = binder preludePath [0, 0] methodName
    methodScheme =
      fixtureClosureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    implId = TypedImplId preludePath capabilityName [TypedBoolType]
    methodArgument =
      resolved TypedCurrentModule TypedValueNamespace "printArgument"
    methodExpression =
      TypedLambdaExpr
        boolToBoolInfo
        (binder preludePath [1, 0, 0] methodArgument)
        methodArgument
        trueExpr
    methodDefinition =
      TypedMethodDefinition
        (TypedMethodId implId "print!")
        (binder preludePath [1, 0] methodName)
        methodName
        span1
        methodExpression
    preludeModule =
      typedModule
        preludePath
        (TypedSourcePath "src/Prelude.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Printable"]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface classDeclaration]
            [TypedImplInterface implId]
        )
        [ TypedClassStatement classDeclaration,
          TypedImplStatement (TypedImplDeclaration span1 implId [methodDefinition])
        ]
        unitInfo
    importedCapabilityName =
      resolved TypedAmbientPrelude TypedCapabilityNamespace "Printable"
    importedImplId =
      TypedImplId preludePath importedCapabilityName [TypedBoolType]
    evidenceUse =
      TypedEvidenceUse
        Nothing
        (TypedCapabilityConstraint (preludeCapability "Printable") (Just "Printable::print!") TypedBoolType)
        importedImplId
        (Just (TypedMethodId importedImplId "print!"))
    methodInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        []
        [TypedSelectedEvidence evidenceUse]
    expression =
      TypedTypeApplicationExpr
        methodInfo
        (fixtureVariableExpr methodInfo (TypedBuiltinName "Printable::print!"))
        span1
        TypedBoolType
    entryModule =
      typedModule
        entryPath
        relativeSource
        []
        []
        emptyInterface
        [expressionStatement 1 expression]
        methodInfo

qualifiedMethodValueContractProgram :: TypedProgram
qualifiedMethodValueContractProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-qualified-method-value-contract"
    capabilityName =
      resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    implId =
      TypedImplId ["Prelude"] capabilityName [TypedTextType]
    evidenceUse =
      TypedEvidenceUse
        Nothing
        (TypedCapabilityConstraint (preludeCapability "Render") (Just "Render::map") TypedTextType)
        implId
        (Just (TypedMethodId implId "map"))
    expression =
      fixtureVariableExpr
        (TypedNodeInfo boolToBoolType boolToBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBuiltinName "Render::map")

aliasShapedSelfRecursionProgram :: TypedProgram
aliasShapedSelfRecursionProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-alias-shaped-self-recursion"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    owner = binder modulePath [0] valueName
    expression =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [ TypedCaseArm
            (TypedWildcardPattern boolInfo)
            Nothing
            (fixtureBoundVariableExpr owner boolInfo valueName)
        ]
    statement =
      TypedLetStatement owner valueName span1 (monoScheme owner) expression

eagerSelfReferenceName :: TypedCoreName
eagerSelfReferenceName =
  resolved TypedCurrentModule TypedValueNamespace "item"

eagerSelfReferenceProgram :: TypedProgram
eagerSelfReferenceProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-eager-self-reference"
    modulePath = (fixtureModulePath fixture)
    owner = binder modulePath [0] eagerSelfReferenceName
    expression =
      TypedIfExpr
        boolInfo
        (fixtureVariableExpr boolInfo eagerSelfReferenceName)
        trueExpr
        falseExpr
    statement =
      TypedLetStatement owner eagerSelfReferenceName span1 (monoScheme owner) expression

importNameCollisionProgram :: TypedProgram
importNameCollisionProgram =
  TypedProgram Nothing [firstLibrary, secondLibrary, entryModule] entryPath
  where
    fixture = "review-import-name-collision"
    firstPath = (fixtureLibraryPath "FirstCollision")
    secondPath = (fixtureLibraryPath "SecondCollision")
    entryPath = (fixtureModulePath fixture)
    collisionLibrary libraryPath constructorIdentifier =
      let dataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
          constructorName = resolved TypedCurrentModule TypedConstructorNamespace constructorIdentifier
          constructorOwner = binder libraryPath [0, 0] constructorName
          declaration =
            TypedDataDeclaration
              span1
              dataName
              []
              [TypedConstructorDeclaration constructorOwner constructorName [] []]
          valueName = resolved TypedCurrentModule TypedValueNamespace "shared"
          valueOwner = binder libraryPath [1] valueName
          valueScheme = monoScheme valueOwner
       in typedModule
            libraryPath
            (TypedSourcePath ("src/" <> Text.intercalate "/" libraryPath <> ".jz"))
            []
            [ TypedModuleExport TypedValueNamespace "shared",
              TypedModuleExport TypedTypeNamespace "Box"
            ]
            ( TypedModuleInterface
                [TypedValueInterface valueName valueScheme]
                [TypedDataInterface declaration]
                []
                []
            )
            [ TypedDataStatement declaration,
              TypedLetStatement valueOwner valueName span1 valueScheme trueExpr
            ]
            unitInfo
    firstLibrary = collisionLibrary firstPath "FirstBox"
    secondLibrary = collisionLibrary secondPath "SecondBox"
    entryModule =
      typedModule
        entryPath
        relativeSource
        [ TypedResolvedImport span1 firstPath Nothing Nothing,
          TypedResolvedImport span1 secondPath Nothing Nothing
        ]
        []
        emptyInterface
        [expressionStatement 1 trueExpr]
        boolInfo

localClassMethodName :: TypedCoreName
localClassMethodName =
  resolved TypedCurrentModule TypedValueNamespace "render"

localClassMethodVisibilityProgram :: TypedProgram
localClassMethodVisibilityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-local-class-method-visibility"
    modulePath = (fixtureModulePath fixture)
    className = resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    methodOwner = binder modulePath [0, 0] localClassMethodName
    methodScheme =
      fixtureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        className
        [TypedTypeParameterId 0]
        [TypedMethodSignature localClassMethodName span1 methodScheme]
    statements =
      [ TypedClassStatement classDeclaration,
        expressionStatement 1 (fixtureVariableExpr boolToBoolInfo localClassMethodName)
      ]

syntheticBinderShadowingProgram :: TypedProgram
syntheticBinderShadowingProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
  where
    fixture = "review-synthetic-binder-shadowing"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    earlierOwner = binder modulePath [99] valueName
    laterOwner = binder modulePath [0] valueName
    earlierScheme = monoScheme earlierOwner
    laterScheme =
      fixtureScheme laterOwner [] [] [] TypedTextType TypedManagedTextRecipe
    statements =
      [ TypedLetStatement earlierOwner valueName span1 earlierScheme trueExpr,
        TypedLetStatement
          laterOwner
          valueName
          span1
          laterScheme
          (TypedLiteralExpr textInfo (TypedTextLiteral "later")),
        expressionStatement 1 (fixtureBoundVariableExpr laterOwner textInfo valueName)
      ]

implFreeClassParameterProgram :: TypedProgram
implFreeClassParameterProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] [statement] emptyInterface unitInfo modulePath)
  where
    fixture = "review-impl-free-class-parameter"
    modulePath = (fixtureModulePath fixture)
    capabilityName =
      resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    methodName = resolved TypedCurrentModule TypedValueNamespace "equal"
    methodOwner = binder modulePath [0, 0] methodName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    identityArgumentName =
      resolved TypedCurrentModule TypedValueNamespace "identityArgument"
    identityArgumentOwner =
      binder modulePath [0, 0, 0] identityArgumentName
    identityInfo =
      info
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    identityExpression =
      TypedLambdaExpr
        identityInfo
        identityArgumentOwner
        identityArgumentName
        ( fixtureBoundVariableExpr
            identityArgumentOwner
            (info parameterType parameterRecipe)
            identityArgumentName
        )
    methodExpression =
      TypedBlockExpr
        boolInfo
        [ expressionStatement 1 identityExpression,
          expressionStatement 2 trueExpr
        ]
    method =
      TypedMethodDefinition
        (TypedMethodId implId "equal")
        methodOwner
        methodName
        span1
        methodExpression
    statement =
      TypedImplStatement
        ( TypedImplDeclaration
            span1
            implId
            [method, fixtureImplMethod modulePath [0, 1] implId "other"]
        )

duplicateQualifiedMethodCandidateImpl :: TypedImplId
duplicateQualifiedMethodCandidateImpl =
  TypedImplId
    ["Prelude"]
    (resolved TypedAmbientPrelude TypedCapabilityNamespace "Render")
    [TypedTextType]

duplicateQualifiedMethodCandidateProgram :: TypedProgram
duplicateQualifiedMethodCandidateProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-duplicate-qualified-method-candidate"
    constraint =
      TypedCapabilityConstraint (preludeCapability "Render") (Just "Render.map") TypedTextType
    candidate =
      TypedEvidenceCandidate
        duplicateQualifiedMethodCandidateImpl
        (Just (TypedMethodId duplicateQualifiedMethodCandidateImpl "map"))
    expression =
      fixtureVariableExpr
        ( TypedNodeInfo
            builtinMapType
            builtinMapRecipe
            []
            [TypedEvidenceCandidates constraint [candidate, candidate]]
        )
        (TypedBuiltinName "map")

metadataOnlyImportedTypeName :: TypedCoreName
metadataOnlyImportedTypeName =
  resolved
    (TypedImportedModule (fixtureLibraryPath "MetadataProvider"))
    TypedTypeNamespace
    "Box"

metadataOnlySourceTypeProgram :: TypedProgram
metadataOnlySourceTypeProgram =
  TypedProgram Nothing [providerModule, entryModule] entryPath
  where
    fixture = "review-metadata-only-source-type"
    providerPath = (fixtureLibraryPath "MetadataProvider")
    entryPath = (fixtureModulePath fixture)
    localDataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    localConstructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Box"
    constructorOwner = binder providerPath [0, 0] localConstructorName
    dataDeclaration =
      TypedDataDeclaration
        span1
        localDataName
        []
        [TypedConstructorDeclaration constructorOwner localConstructorName [] []]
    localDataType = TypedDataType localDataName []
    localDataRecipe = TypedManagedVariantRecipe localDataName []
    localDataInfo = info localDataType localDataRecipe
    providerValueName =
      resolved TypedCurrentModule TypedValueNamespace "make"
    providerValueOwner = binder providerPath [1] providerValueName
    providerValueScheme =
      fixtureScheme
        providerValueOwner
        []
        []
        []
        localDataType
        localDataRecipe
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/MetadataProvider.jz")
        []
        [TypedModuleExport TypedValueNamespace "make"]
        ( TypedModuleInterface
            [TypedValueInterface providerValueName providerValueScheme]
            [TypedDataInterface dataDeclaration]
            []
            []
        )
        [ TypedDataStatement dataDeclaration,
          TypedLetStatement
            providerValueOwner
            providerValueName
            span1
            providerValueScheme
            (fixtureBoundVariableExpr constructorOwner localDataInfo localConstructorName)
        ]
        unitInfo
    leakedValueName =
      resolved TypedCurrentModule TypedValueNamespace "leaked"
    leakedValueOwner = binder entryPath [0] leakedValueName
    leakedScheme =
      fixtureScheme
        leakedValueOwner
        []
        []
        []
        (TypedDataType metadataOnlyImportedTypeName [])
        (TypedManagedVariantRecipe metadataOnlyImportedTypeName [])
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 providerPath Nothing (Just ["make"])]
        []
        emptyInterface
        [TypedSignatureStatement leakedValueOwner leakedValueName span1 leakedScheme]
        unitInfo

nonScalarCharacterProgram :: TypedProgram
nonScalarCharacterProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-non-scalar-character"
    modulePath = (fixtureModulePath fixture)
    charInfo = info TypedCharType TypedCharRecipe
    nonScalar = '\xD800'
    invalidLiteral =
      TypedLiteralExpr charInfo (TypedCharacterLiteral nonScalar)
    invalidPattern =
      TypedLiteralPattern charInfo (TypedCharacterLiteral nonScalar)
    patternExpression =
      TypedPatternCaseExpr
        boolInfo
        (TypedLiteralExpr charInfo (TypedCharacterLiteral 'x'))
        [TypedCaseArm invalidPattern Nothing trueExpr]
    statements =
      [ expressionStatement 1 invalidLiteral,
        expressionStatement 2 patternExpression
      ]

inferredMethodOnlyCapabilityVisibilityProgram :: TypedProgram
inferredMethodOnlyCapabilityVisibilityProgram =
  methodOnlyCapabilityVisibilityProgram False

explicitMethodOnlyCapabilityVisibilityProgram :: TypedProgram
explicitMethodOnlyCapabilityVisibilityProgram =
  methodOnlyCapabilityVisibilityProgram True

methodOnlyCapabilityVisibilityProgram :: Bool -> TypedProgram
methodOnlyCapabilityVisibilityProgram hasExplicitSignature =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    fixture = "review-method-only-capability-visibility"
    libraryPath = (fixtureLibraryPath "MethodOnlyCapability")
    entryPath = (fixtureModulePath fixture)
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    importedCapabilityName =
      resolved
        (TypedImportedModule libraryPath)
        TypedCapabilityNamespace
        "Render"
    methodName =
      resolved TypedCurrentModule TypedValueNamespace "render"
    methodOwner = binder libraryPath [0, 0] methodName
    methodScheme =
      fixtureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        [TypedMethodSignature methodName span1 methodScheme]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/MethodOnlyCapability.jz")
        []
        [ TypedModuleExport TypedCapabilityNamespace "Render",
          TypedModuleExport TypedValueNamespace "render"
        ]
        (TypedModuleInterface [] [] [TypedClassInterface classDeclaration] [])
        [TypedClassStatement classDeclaration]
        unitInfo
    localName = resolved TypedCurrentModule TypedValueNamespace "local"
    inferredOwner = binder entryPath [0] localName
    explicitOwner = binder entryPath [0] localName
    explicitBindingOwner = binder entryPath [1] localName
    localScheme owner =
      fixtureScheme
        owner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint importedCapabilityName Nothing TypedBoolType)
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    localStatements
      | hasExplicitSignature =
          [ TypedSignatureStatement explicitOwner localName span1 (localScheme explicitOwner),
            TypedLetStatement explicitBindingOwner localName span1 (localScheme explicitBindingOwner) trueExpr
          ]
      | otherwise =
          [TypedLetStatement inferredOwner localName span1 (localScheme inferredOwner) trueExpr]
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["render"])]
        []
        emptyInterface
        localStatements
        boolInfo

capabilityImportCollisionProgram :: TypedProgram
capabilityImportCollisionProgram =
  TypedProgram Nothing [valueModule, capabilityModule, entryModule] entryPath
  where
    fixture = "review-capability-import-collision"
    valuePath = (fixtureLibraryPath "SharedValue")
    capabilityPath = (fixtureLibraryPath "SharedCapability")
    entryPath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "Shared"
    valueOwner = binder valuePath [0] valueName
    valueScheme = monoScheme valueOwner
    valueModule =
      typedModule
        valuePath
        (TypedSourcePath "src/Library/SharedValue.jz")
        []
        [TypedModuleExport TypedValueNamespace "Shared"]
        (TypedModuleInterface [TypedValueInterface valueName valueScheme] [] [] [])
        [TypedLetStatement valueOwner valueName span1 valueScheme trueExpr]
        boolInfo
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Shared"
    capabilityDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    capabilityModule =
      typedModule
        capabilityPath
        (TypedSourcePath "src/Library/SharedCapability.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Shared"]
        (TypedModuleInterface [] [] [TypedClassInterface capabilityDeclaration] [])
        [TypedClassStatement capabilityDeclaration]
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [ TypedResolvedImport span1 valuePath Nothing Nothing,
          TypedResolvedImport span1 capabilityPath Nothing Nothing
        ]
        []
        emptyInterface
        [expressionStatement 1 trueExpr]
        boolInfo

nestedTypeParameterShadowingProgram :: TypedProgram
nestedTypeParameterShadowingProgram =
  singleModuleProgram fixture relativeSource [] [topLevelBinding] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-type-parameter-shadowing"
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    parameterInfo = info parameterType parameterRecipe
    outerName = resolved TypedCurrentModule TypedValueNamespace "outer"
    outerOwner = binder modulePath [0] outerName
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentOwner = binder modulePath [0, 0] argumentName
    localName = resolved TypedCurrentModule TypedValueNamespace "local"
    localOwner = binder modulePath [0, 0, 0] localName
    localScheme =
      fixtureScheme
        localOwner
        [parameter]
        []
        []
        parameterType
        parameterRecipe
    localBinding =
      TypedLetStatement
        localOwner
        localName
        span1
        localScheme
        (fixtureBoundVariableExpr argumentOwner parameterInfo argumentName)
    localUseInfo =
      TypedNodeInfo
        parameterType
        parameterRecipe
        [ TypedInstantiation
            localOwner
            [TypedTypeArgument parameter parameterType]
            Nothing
        ]
        []
    block =
      TypedBlockExpr
        parameterInfo
        [ localBinding,
          expressionStatement 2 (fixtureVariableExpr localUseInfo localName)
        ]
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe =
      TypedClosureRecipe [parameterRecipe] parameterRecipe
    outerExpression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        argumentOwner
        argumentName
        block
    outerScheme =
      fixtureScheme
        outerOwner
        [parameter]
        []
        []
        functionType
        functionRecipe
    topLevelBinding =
      TypedLetStatement
        outerOwner
        outerName
        span1
        outerScheme
        outerExpression

typeOnlyImportSelectorProgram :: TypedProgram
typeOnlyImportSelectorProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    fixture = "review-type-only-import-selector"
    libraryPath = (fixtureLibraryPath "TypeOnlySelector")
    entryPath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    constructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "BoxValue"
    dataDeclaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [ TypedConstructorDeclaration
            (binder libraryPath [0, 0] constructorName)
            constructorName
            []
            []
        ]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/TypeOnlySelector.jz")
        []
        [TypedModuleExport TypedTypeNamespace "Box"]
        (TypedModuleInterface [] [TypedDataInterface dataDeclaration] [] [])
        [TypedDataStatement dataDeclaration]
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["Box"])]
        []
        emptyInterface
        [expressionStatement 1 trueExpr]
        boolInfo

ordinaryUnboundEvidenceProgram :: TypedProgram
ordinaryUnboundEvidenceProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-ordinary-unbound-evidence"
    capabilityName =
      resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint =
      TypedCapabilityConstraint (preludeCapability "Equal") (Just "Equal.equal") TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    methodId = TypedMethodId implId "equal"
    evidenceUse =
      TypedEvidenceUse Nothing constraint implId (Just methodId)
    expression =
      TypedLiteralExpr
        ( TypedNodeInfo
            TypedBoolType
            TypedBoolRecipe
            []
            [TypedSelectedEvidence evidenceUse]
        )
        (TypedBooleanLiteral True)

nestedLocalGeneralizationProgram :: TypedProgram
nestedLocalGeneralizationProgram =
  singleModuleProgram fixture relativeSource [] [topLevelBinding] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-local-generalization"
    modulePath = (fixtureModulePath fixture)
    outerParameter = TypedTypeParameterId 0
    innerParameter = TypedTypeParameterId 1
    outerParameterType = TypedTypeParameterType outerParameter
    outerParameterRecipe = TypedRepresentationParameterRecipe outerParameter
    outerParameterInfo = info outerParameterType outerParameterRecipe
    innerParameterType = TypedTypeParameterType innerParameter
    innerParameterRecipe = TypedRepresentationParameterRecipe innerParameter
    innerParameterInfo = info innerParameterType innerParameterRecipe
    outerName = resolved TypedCurrentModule TypedValueNamespace "outer"
    outerOwner = binder modulePath [0] outerName
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentOwner = binder modulePath [0, 0] argumentName
    localName = resolved TypedCurrentModule TypedValueNamespace "local"
    localOwner = binder modulePath [0, 0, 0] localName
    localArgumentName =
      resolved TypedCurrentModule TypedValueNamespace "localArgument"
    localArgumentOwner = binder modulePath [0, 0, 0, 0] localArgumentName
    localFunctionType =
      TypedFunctionType innerParameterType innerParameterType
    localFunctionRecipe =
      TypedClosureRecipe [innerParameterRecipe] innerParameterRecipe
    localScheme =
      fixtureScheme
        localOwner
        [innerParameter]
        []
        []
        localFunctionType
        localFunctionRecipe
    localExpression =
      TypedLambdaExpr
        (info localFunctionType localFunctionRecipe)
        localArgumentOwner
        localArgumentName
        (fixtureBoundVariableExpr localArgumentOwner innerParameterInfo localArgumentName)
    localBinding =
      TypedLetStatement
        localOwner
        localName
        span1
        localScheme
        localExpression
    instantiatedLocalType =
      TypedFunctionType outerParameterType outerParameterType
    instantiatedLocalRecipe =
      TypedClosureRecipe [outerParameterRecipe] outerParameterRecipe
    localUseInfo =
      TypedNodeInfo
        instantiatedLocalType
        instantiatedLocalRecipe
        [ TypedInstantiation
            localOwner
            [TypedTypeArgument innerParameter outerParameterType]
            Nothing
        ]
        []
    localUse = fixtureVariableExpr localUseInfo localName
    localApplication =
      TypedApplyExpr
        outerParameterInfo
        localUse
        (fixtureBoundVariableExpr argumentOwner outerParameterInfo argumentName)
    block =
      TypedBlockExpr
        outerParameterInfo
        [ localBinding,
          expressionStatement 2 localApplication
        ]
    functionType =
      TypedFunctionType outerParameterType outerParameterType
    functionRecipe =
      TypedClosureRecipe [outerParameterRecipe] outerParameterRecipe
    outerExpression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        argumentOwner
        argumentName
        block
    outerScheme =
      fixtureScheme
        outerOwner
        [outerParameter]
        []
        []
        functionType
        functionRecipe
    topLevelBinding =
      TypedLetStatement
        outerOwner
        outerName
        span1
        outerScheme
        outerExpression

nonConcreteImplTargetId :: TypedImplId
nonConcreteImplTargetId =
  TypedImplId
    (fixtureModulePath "review-non-concrete-impl-target")
    (resolved TypedCurrentModule TypedCapabilityNamespace "Concrete")
    [TypedFunctionType TypedBoolType TypedBoolType]

nonConcreteImplTargetProgram :: TypedProgram
nonConcreteImplTargetProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-non-concrete-impl-target"
    modulePath = (fixtureModulePath fixture)
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Concrete"
    capabilityDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    statements =
      [ TypedClassStatement capabilityDeclaration,
        TypedImplStatement
          (TypedImplDeclaration span1 nonConcreteImplTargetId []),
        expressionStatement 3 trueExpr
      ]

blockDeclarationScopeProgram :: TypedProgram
blockDeclarationScopeProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-block-declaration-scope"
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Nested"
    constructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Nested"
    dataDeclaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] constructorName)
            constructorName
            []
            []
        ]
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "NestedClass"
    capabilityDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    block =
      TypedBlockExpr
        boolInfo
        [ TypedDataStatement dataDeclaration,
          TypedClassStatement capabilityDeclaration,
          TypedImplStatement
            (TypedImplDeclaration span1 implId []),
          expressionStatement 4 trueExpr
        ]

delimiterModulePathProgram :: TypedProgram
delimiterModulePathProgram =
  modulePathFixtureProgram ["A::B"]

slashModulePathProgram :: TypedProgram
slashModulePathProgram =
  modulePathFixtureProgram ["App/Main"]

reservedModulePathProgram :: TypedProgram
reservedModulePathProgram =
  modulePathFixtureProgram ["if"]

modulePathFixtureProgram :: [Text] -> TypedProgram
modulePathFixtureProgram modulePath =
  TypedProgram
    Nothing
    [ typedModule
        modulePath
        relativeSource
        []
        []
        emptyInterface
        [expressionStatement 1 trueExpr]
        boolInfo
    ]
    modulePath

moduleMetadataIdentityOwner :: TypedBinderId
moduleMetadataIdentityOwner =
  binder
    (fixtureModulePath "review-module-metadata-identity")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "item")

moduleMetadataIdentityProgram :: TypedProgram
moduleMetadataIdentityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface moduleInfo modulePath
  where
    fixture = "review-module-metadata-identity"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    owner = moduleMetadataIdentityOwner
    scheme = monoScheme owner
    moduleInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [TypedInstantiation owner [] Nothing]
        []
    statements =
      [ TypedLetStatement owner valueName span1 scheme trueExpr,
        expressionStatement 2 trueExpr
      ]

qualifiedTypeApplicationInstantiationOwner :: TypedBinderId
qualifiedTypeApplicationInstantiationOwner =
  binder
    (fixtureModulePath "review-qualified-type-application-instantiation")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "ordinary")

qualifiedTypeApplicationInstantiationProgram :: TypedProgram
qualifiedTypeApplicationInstantiationProgram =
  case qualifiedMethodTypeApplicationProgram of
    TypedProgram
      prelude
      [TypedModule _ sourcePath imports exports interface _ [TypedExpressionStatement expressionSpan originalExpression] _]
      _ ->
        case originalExpression of
          TypedTypeApplicationExpr (TypedNodeInfo resultType resultRecipe [] evidence) function explicitSpan typeArgument ->
            let applicationInfo =
                  TypedNodeInfo
                    resultType
                    resultRecipe
                    [TypedInstantiation qualifiedTypeApplicationInstantiationOwner [] Nothing]
                    evidence
                expression =
                  TypedTypeApplicationExpr applicationInfo function explicitSpan typeArgument
                ordinaryName =
                  resolved TypedCurrentModule TypedValueNamespace "ordinary"
                ordinaryScheme =
                  monoScheme qualifiedTypeApplicationInstantiationOwner
                entryPath =
                  (fixtureModulePath "review-qualified-type-application-instantiation")
                entryModule =
                  TypedModule
                    entryPath
                    sourcePath
                    imports
                    exports
                    interface
                    []
                    [ TypedLetStatement
                        qualifiedTypeApplicationInstantiationOwner
                        ordinaryName
                        span1
                        ordinaryScheme
                        trueExpr,
                      TypedExpressionStatement expressionSpan expression
                    ]
                    applicationInfo
             in TypedProgram prelude [entryModule] entryPath
          _ -> error "qualified method type-application fixture changed shape"
    _ -> error "qualified method type-application program changed shape"

localClassMethodAfterValueProgram :: TypedProgram
localClassMethodAfterValueProgram =
  localClassMethodSchemeProgram "review-local-class-method-after-value" False

localClassMethodBeforeValueProgram :: TypedProgram
localClassMethodBeforeValueProgram =
  localClassMethodSchemeProgram "review-local-class-method-before-value" True

localClassMethodSchemeProgram :: Text -> Bool -> TypedProgram
localClassMethodSchemeProgram fixture classFirst =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "shared"
    valueOwner = binder modulePath [0] valueName
    valueStatement =
      TypedLetStatement valueOwner valueName span1 (monoScheme valueOwner) trueExpr
    className =
      resolved TypedCurrentModule TypedCapabilityNamespace "SharedClass"
    methodOwner = binder modulePath [1, 0] valueName
    methodScheme =
      fixtureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    classStatement =
      TypedClassStatement
        ( TypedClassDeclaration
            span1
            className
            [TypedTypeParameterId 0]
            [TypedMethodSignature valueName span1 methodScheme]
        )
    declarations
      | classFirst = [classStatement, valueStatement]
      | otherwise = [valueStatement, classStatement]
    statements =
      declarations
        <> [expressionStatement 3 (fixtureBoundVariableExpr valueOwner boolInfo valueName)]

lexicalSchemeShadowingProgram :: TypedProgram
lexicalSchemeShadowingProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
  where
    fixture = "review-lexical-scheme-shadowing"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    outerOwner = binder modulePath [0] valueName
    innerOwner = binder modulePath [1, 0] valueName
    innerUse = fixtureBoundVariableExpr innerOwner textInfo valueName
    block =
      TypedBlockExpr
        textInfo
        [ TypedLetStatement
            innerOwner
            valueName
            span1
            (fixtureScheme innerOwner [] [] [] TypedTextType TypedManagedTextRecipe)
            (TypedLiteralExpr textInfo (TypedTextLiteral "inner")),
          expressionStatement 2 innerUse
        ]
    statements =
      [ TypedLetStatement outerOwner valueName span1 (monoScheme outerOwner) trueExpr,
        expressionStatement 1 block
      ]

fullyAppliedMethodCandidatesProgram :: TypedProgram
fullyAppliedMethodCandidatesProgram =
  qualifiedMapDispatchProgram
    fixture
    []
    [ TypedEvidenceCandidates
        fixtureRenderConstraint
        [ fixtureRenderCandidate (fixtureRenderImpl ["Prelude"]),
          fixtureRenderCandidate (fixtureRenderImpl (fixtureModulePath fixture))
        ]
    ]
  where
    fixture = "review-fully-applied-method-candidates"

fixtureRenderImpl :: [Text] -> TypedImplId
fixtureRenderImpl modulePath =
  TypedImplId
    modulePath
    (resolved TypedAmbientPrelude TypedCapabilityNamespace "Render")
    [TypedTextType]

fixtureRenderConstraint :: TypedCapabilityConstraint
fixtureRenderConstraint =
  TypedCapabilityConstraint
    (preludeCapability "Render")
    (Just "Render.map")
    TypedTextType

fixtureRenderCandidate :: TypedImplId -> TypedEvidenceCandidate
fixtureRenderCandidate implId =
  TypedEvidenceCandidate implId (Just (TypedMethodId implId "map"))

qualifiedMapDispatchProgram :: Text -> [TypedEvidenceSelection] -> [TypedEvidenceSelection] -> TypedProgram
qualifiedMapDispatchProgram fixture intermediateEvidence resultEvidence =
  withFixturePrelude
    ( singleModuleProgram
        fixture
        relativeSource
        []
        [ TypedImplStatement
            ( TypedImplDeclaration
                span1
                secondImpl
                [ fixtureImplMethod modulePath [0, 0] secondImpl "render",
                  fixtureImplMethod modulePath [0, 1] secondImpl "map"
                ]
            ),
          expressionStatement 1 expression
        ]
        emptyInterface
        resultInfo
        modulePath
    )
  where
    modulePath = fixtureModulePath fixture
    secondImpl = fixtureRenderImpl modulePath
    boolToTextType = TypedFunctionType TypedBoolType TypedTextType
    boolToTextRecipe = TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe
    mapperName = resolved TypedCurrentModule TypedValueNamespace "mapperArgument"
    mapper =
      TypedLambdaExpr
        (info boolToTextType boolToTextRecipe)
        (binder modulePath [1, 0] mapperName)
        mapperName
        (TypedLiteralExpr textInfo (TypedTextLiteral "mapped"))
    intermediateType =
      TypedFunctionType
        (TypedListType TypedBoolType)
        (TypedListType TypedTextType)
    intermediateRecipe =
      TypedClosureRecipe
        [TypedManagedListRecipe TypedBoolRecipe]
        (TypedManagedListRecipe TypedManagedTextRecipe)
    intermediate =
      TypedApplyExpr
        (TypedNodeInfo intermediateType intermediateRecipe [] intermediateEvidence)
        (fixtureVariableExpr builtinMapDirectInfo (TypedBuiltinName "map"))
        mapper
    argument =
      TypedListExpr
        (info (TypedListType TypedBoolType) (TypedManagedListRecipe TypedBoolRecipe))
        [trueExpr]
    resultInfo =
      TypedNodeInfo
        (TypedListType TypedTextType)
        (TypedManagedListRecipe TypedManagedTextRecipe)
        []
        resultEvidence
    expression = TypedApplyExpr resultInfo intermediate argument

duplicateUnboundEvidenceProgram :: TypedProgram
duplicateUnboundEvidenceProgram =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    fixture = "review-duplicate-unbound-evidence"
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") (Just "Equal.equal") TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    use =
      TypedEvidenceUse
        Nothing
        constraint
        implId
        (Just (TypedMethodId implId "equal"))
    expression =
      fixtureVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence use, TypedSelectedEvidence use])
        (TypedBuiltinName "Equal::equal")

generalizedClassMethodImportProgram :: TypedProgram
generalizedClassMethodImportProgram =
  generalizedClassMethodImportProgramWith
    "review-generalized-class-method-import"
    True

missingImportedClassMethodDispatchProgram :: TypedProgram
missingImportedClassMethodDispatchProgram =
  generalizedClassMethodImportProgramWith
    "review-missing-imported-class-method-dispatch"
    False

generalizedClassMethodImportProgramWith :: Text -> Bool -> TypedProgram
generalizedClassMethodImportProgramWith fixture includeEvidence =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "GeneralizedClassMethod")
    entryPath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    className = resolved TypedCurrentModule TypedCapabilityNamespace "Display"
    methodName = resolved TypedCurrentModule TypedValueNamespace "display"
    methodOwner = binder libraryPath [0, 0] methodName
    methodType =
      TypedFunctionType
        (TypedTypeParameterType parameter)
        (TypedTypeParameterType parameter)
    methodRecipe =
      TypedClosureRecipe
        [TypedRepresentationParameterRecipe parameter]
        (TypedRepresentationParameterRecipe parameter)
    methodScheme =
      fixtureClosureScheme methodOwner [] [] [] methodType methodRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        className
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    localImplId = TypedImplId libraryPath className [TypedBoolType]
    methodDefinition =
      fixtureImplMethod libraryPath [1, 0] localImplId "display"
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/GeneralizedClassMethod.jz")
        []
        [ TypedModuleExport TypedCapabilityNamespace "Display",
          TypedModuleExport TypedValueNamespace "display"
        ]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface classDeclaration]
            [TypedImplInterface localImplId]
        )
        [ TypedClassStatement classDeclaration,
          TypedImplStatement
            (TypedImplDeclaration span1 localImplId [methodDefinition])
        ]
        unitInfo
    importedMethodName =
      resolved (TypedImportedModule libraryPath) TypedValueNamespace "display"
    importedCapabilityName =
      resolved
        (TypedImportedModule libraryPath)
        TypedCapabilityNamespace
        "Display"
    importedImplId =
      TypedImplId libraryPath importedCapabilityName [TypedBoolType]
    constraint =
      TypedCapabilityConstraint
        importedCapabilityName
        (Just (Text.intercalate "::" (libraryPath <> ["Display", "display"])))
        TypedBoolType
    selectedEvidence =
      TypedSelectedEvidence
        ( TypedEvidenceUse
            ( Just
                ( TypedEvidenceParameterRef
                    methodOwner
                    (TypedEvidenceParameterId 0)
                )
            )
            constraint
            importedImplId
            (Just (TypedMethodId importedImplId "display"))
        )
    evidence
      | includeEvidence = [selectedEvidence]
      | otherwise = []
    instantiatedInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        [ TypedInstantiation
            methodOwner
            [TypedTypeArgument parameter TypedBoolType]
            Nothing
        ]
        evidence
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["display"])]
        []
        emptyInterface
        [expressionStatement 1 (fixtureVariableExpr instantiatedInfo importedMethodName)]
        instantiatedInfo

importedClassCollisionProgram :: TypedProgram
importedClassCollisionProgram =
  TypedProgram Nothing [firstLibrary, secondLibrary, entryModule] entryPath
  where
    firstPath = (fixtureLibraryPath "FirstClash")
    secondPath = (fixtureLibraryPath "SecondClash")
    entryPath = (fixtureModulePath "review-imported-class-collision")
    parameter = TypedTypeParameterId 0
    libraryModule libraryPath =
      let className = resolved TypedCurrentModule TypedCapabilityNamespace "Clash"
          declaration = TypedClassDeclaration span1 className [parameter] []
       in typedModule
            libraryPath
            (TypedSourcePath ("src/" <> Text.intercalate "/" libraryPath <> ".jz"))
            []
            [TypedModuleExport TypedCapabilityNamespace "Clash"]
            (TypedModuleInterface [] [] [TypedClassInterface declaration] [])
            [TypedClassStatement declaration]
            unitInfo
    firstLibrary = libraryModule firstPath
    secondLibrary = libraryModule secondPath
    valueName = resolved TypedCurrentModule TypedValueNamespace "constrained"
    valueOwner = binder entryPath [0] valueName
    constraint =
      TypedCapabilityConstraint
        (resolved (TypedImportedModule firstPath) TypedCapabilityNamespace "Clash")
        Nothing
        TypedBoolType
    scheme =
      fixtureScheme
        valueOwner
        []
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    entryModule =
      typedModule
        entryPath
        relativeSource
        [ TypedResolvedImport span1 firstPath Nothing (Just ["Clash"]),
          TypedResolvedImport span1 secondPath Nothing (Just ["Clash"])
        ]
        []
        emptyInterface
        [ TypedLetStatement valueOwner valueName span1 scheme trueExpr,
          expressionStatement 1 trueExpr
        ]
        boolInfo

forwardBlockReferenceName :: TypedCoreName
forwardBlockReferenceName =
  resolved TypedCurrentModule TypedValueNamespace "later"

forwardBlockReferenceProgram :: TypedProgram
forwardBlockReferenceProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-forward-block-reference"
    modulePath = (fixtureModulePath fixture)
    owner = binder modulePath [0, 1] forwardBlockReferenceName
    block =
      TypedBlockExpr
        boolInfo
        [ expressionStatement 2 (fixtureVariableExpr boolInfo forwardBlockReferenceName),
          TypedLetStatement owner forwardBlockReferenceName span1 (monoScheme owner) trueExpr,
          expressionStatement 3 trueExpr
        ]

recursiveBlockPeerProgram :: TypedProgram
recursiveBlockPeerProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-recursive-block-peers"
    modulePath = (fixtureModulePath fixture)
    leftName = resolved TypedCurrentModule TypedValueNamespace "left"
    rightName = resolved TypedCurrentModule TypedValueNamespace "right"
    leftOwner = binder modulePath [0, 0] leftName
    rightOwner = binder modulePath [0, 1] rightName
    recursiveLambda ownerPath argumentName peerOwner peerName =
      let argumentOwner = binder modulePath ownerPath argumentName
       in TypedLambdaExpr
            boolToBoolInfo
            argumentOwner
            argumentName
            ( TypedApplyExpr
                boolInfo
                (fixtureBoundVariableExpr peerOwner boolToBoolInfo peerName)
                (fixtureBoundVariableExpr argumentOwner boolInfo argumentName)
            )
    leftArgument = resolved TypedCurrentModule TypedValueNamespace "leftArgument"
    rightArgument = resolved TypedCurrentModule TypedValueNamespace "rightArgument"
    leftStatement =
      TypedLetStatement
        leftOwner
        leftName
        span1
        (fixtureClosureScheme leftOwner [] [] [] boolToBoolType boolToBoolRecipe)
        (recursiveLambda [0, 0, 0] leftArgument rightOwner rightName)
    rightStatement =
      TypedLetStatement
        rightOwner
        rightName
        span1
        (fixtureClosureScheme rightOwner [] [] [] boolToBoolType boolToBoolRecipe)
        (recursiveLambda [0, 1, 0] rightArgument leftOwner leftName)
    block =
      TypedBlockExpr
        boolToBoolInfo
        [ leftStatement,
          rightStatement,
          expressionStatement 3 (fixtureBoundVariableExpr leftOwner boolToBoolInfo leftName)
        ]

recursiveGroupContractCases :: [(Text, TypedProgram, [TypedCoreValidationFailure])]
recursiveGroupContractCases =
  [ ( "review-recursive-group-direct-self",
      recursiveGroupDirectSelfProgram,
      []
    ),
    ( "review-recursive-group-direct-mutual",
      recursiveGroupDirectMutualProgram,
      []
    ),
    ( "review-recursive-group-empty",
      recursiveGroupEmptyProgram,
      [moduleFailure "review-recursive-group-empty" TypedRecursiveGroupMismatch (TypedIndexDetail 0)]
    ),
    ( "review-recursive-group-unknown-member",
      recursiveGroupUnknownMemberProgram,
      [ moduleFailure
          "review-recursive-group-unknown-member"
          TypedUnknownBinder
          (TypedBinderDetail (recursiveGroupOwner "review-recursive-group-unknown-member" 9))
      ]
    ),
    ( "review-recursive-group-duplicate-member",
      recursiveGroupDuplicateMemberProgram,
      [ statementFailure
          "review-recursive-group-duplicate-member"
          0
          TypedDuplicateBinder
          (TypedBinderDetail (recursiveGroupOwner "review-recursive-group-duplicate-member" 0))
      ]
    ),
    ( "review-recursive-group-multiple-membership",
      recursiveGroupMultipleMembershipProgram,
      [ statementFailure
          "review-recursive-group-multiple-membership"
          0
          TypedDuplicateBinder
          (TypedBinderDetail (recursiveGroupOwner "review-recursive-group-multiple-membership" 0))
      ]
    ),
    ( "review-recursive-group-member-order",
      recursiveGroupMemberOrderProgram,
      [moduleFailure "review-recursive-group-member-order" TypedRecursiveGroupMismatch (TypedIndexDetail 0)]
    ),
    ( "review-recursive-group-order",
      recursiveGroupOrderProgram,
      [moduleFailure "review-recursive-group-order" TypedRecursiveGroupMismatch (TypedIndexDetail 1)]
    ),
    ( "review-recursive-group-missing-cycle",
      recursiveGroupMissingCycleProgram,
      [ statementFailure
          "review-recursive-group-missing-cycle"
          1
          TypedRecursiveGroupMismatch
          (TypedBinderDetail (recursiveGroupOwnerAt "review-recursive-group-missing-cycle" 1 "first"))
      ]
    ),
    ( "review-recursive-group-spurious-cycle",
      recursiveGroupSpuriousCycleProgram,
      [ statementFailure
          "review-recursive-group-spurious-cycle"
          0
          TypedRecursiveGroupMismatch
          (TypedBinderDetail (recursiveGroupOwner "review-recursive-group-spurious-cycle" 0))
      ]
    ),
    ( "review-recursive-group-mixed-shapes",
      recursiveGroupMixedShapesProgram,
      [ statementFailure
          "review-recursive-group-mixed-shapes"
          0
          TypedRecursiveGroupMismatch
          (TypedBinderDetail (recursiveGroupOwner "review-recursive-group-mixed-shapes" 0))
      ]
    )
  ]

recursiveGroupFixCases :: [(Text, TypedProgram, [TypedCoreValidationFailure])]
recursiveGroupFixCases =
  [ ( "review-recursive-group-earliest-member-order",
      recursiveGroupEarliestMemberOrderProgram,
      [ moduleFailure
          "review-recursive-group-earliest-member-order"
          TypedRecursiveGroupMismatch
          (TypedIndexDetail 0)
      ]
    ),
    ( "review-recursive-group-non-callable-visibility",
      recursiveGroupNonCallableVisibilityProgram,
      [ moduleFailure
          "review-recursive-group-non-callable-visibility"
          TypedUnknownBinder
          (TypedBinderDetail (recursiveGroupOwner "review-recursive-group-non-callable-visibility" 1)),
        TypedCoreValidationFailure
          (TypedExpressionPath (fixtureModulePath "review-recursive-group-non-callable-visibility") [0] [0, 0])
          TypedInvisibleName
          (TypedNameDetail (fixtureValueName "function1")),
        TypedCoreValidationFailure
          (TypedExpressionPath (fixtureModulePath "review-recursive-group-non-callable-visibility") [0] [0, 0])
          TypedBinderReferenceMismatch
          (TypedBinderDetail (recursiveGroupOwner "review-recursive-group-non-callable-visibility" 1))
      ]
    ),
    ( "review-recursive-group-first-overlap-visibility",
      recursiveGroupFirstOverlapVisibilityProgram,
      [ statementFailure
          "review-recursive-group-first-overlap-visibility"
          0
          TypedDuplicateBinder
          (TypedBinderDetail (recursiveGroupOwner "review-recursive-group-first-overlap-visibility" 0))
      ]
    ),
    ( "review-recursive-group-duplicate-callable-identity",
      recursiveGroupDuplicateCallableIdentityProgram,
      [ statementFailure
          "review-recursive-group-duplicate-callable-identity"
          1
          TypedDuplicateBinder
          (TypedBinderDetail (recursiveGroupOwner "review-recursive-group-duplicate-callable-identity" 0))
      ]
    )
  ]

recursiveGroupEarliestMemberOrderProgram :: TypedProgram
recursiveGroupEarliestMemberOrderProgram =
  recursiveGroupProgram
    "review-recursive-group-earliest-member-order"
    [TypedDirectCallableShape, TypedDirectCallableShape, TypedDirectCallableShape]
    [Just 0, Just 1, Just 2]
    [[2, 0], [1]]

recursiveGroupNonCallableVisibilityProgram :: TypedProgram
recursiveGroupNonCallableVisibilityProgram =
  TypedProgram Nothing [moduleValue] modulePath
  where
    fixture = "review-recursive-group-non-callable-visibility"
    modulePath = fixtureModulePath fixture
    callableName = fixtureValueName "function0"
    callableOwner = recursiveGroupOwner fixture 0
    scalarName = fixtureValueName "function1"
    scalarOwner = recursiveGroupOwner fixture 1
    callableScheme = recursiveGroupScheme callableOwner TypedDirectCallableShape
    argumentName = fixtureValueName "argument0"
    argumentOwner = binder modulePath [0, 0] argumentName
    callableExpression =
      TypedLambdaExpr
        boolToBoolInfo
        argumentOwner
        argumentName
        (fixtureBoundVariableExpr scalarOwner boolInfo scalarName)
    statements =
      [ TypedLetStatement callableOwner callableName span1 callableScheme callableExpression,
        TypedLetStatement scalarOwner scalarName span1 (monoScheme scalarOwner) trueExpr
      ]
    moduleValue =
      TypedModule
        modulePath
        relativeSource
        []
        []
        emptyInterface
        [TypedRecursiveGroup [callableOwner, scalarOwner]]
        statements
        unitInfo

recursiveGroupFirstOverlapVisibilityProgram :: TypedProgram
recursiveGroupFirstOverlapVisibilityProgram =
  recursiveGroupProgram
    "review-recursive-group-first-overlap-visibility"
    [TypedDirectCallableShape, TypedDirectCallableShape, TypedDirectCallableShape]
    [Just 1, Nothing, Nothing]
    [[0, 1], [0, 2]]

recursiveGroupDuplicateCallableIdentityProgram :: TypedProgram
recursiveGroupDuplicateCallableIdentityProgram =
  TypedProgram Nothing [moduleValue] modulePath
  where
    fixture = "review-recursive-group-duplicate-callable-identity"
    modulePath = fixtureModulePath fixture
    functionName = fixtureValueName "function0"
    functionOwner = recursiveGroupOwner fixture 0
    functionScheme = recursiveGroupScheme functionOwner TypedDirectCallableShape
    firstStatement =
      recursiveGroupCallable
        modulePath
        0
        functionOwner
        functionName
        functionScheme
        functionOwner
        functionName
    secondStatement =
      recursiveGroupIdentityCallable modulePath 1 functionOwner functionName functionScheme
    moduleValue =
      TypedModule
        modulePath
        relativeSource
        []
        []
        emptyInterface
        [TypedRecursiveGroup [functionOwner]]
        [firstStatement, secondStatement]
        unitInfo

recursiveGroupDirectSelfProgram :: TypedProgram
recursiveGroupDirectSelfProgram =
  recursiveGroupProgram
    "review-recursive-group-direct-self"
    [TypedDirectCallableShape]
    [Just 0]
    [[0]]

recursiveGroupDirectMutualProgram :: TypedProgram
recursiveGroupDirectMutualProgram =
  recursiveGroupProgram
    "review-recursive-group-direct-mutual"
    [TypedDirectCallableShape, TypedDirectCallableShape]
    [Just 1, Just 0]
    [[0, 1]]

recursiveGroupEmptyProgram :: TypedProgram
recursiveGroupEmptyProgram =
  recursiveGroupProgram
    "review-recursive-group-empty"
    [TypedDirectCallableShape]
    [Nothing]
    [[]]

recursiveGroupUnknownMemberProgram :: TypedProgram
recursiveGroupUnknownMemberProgram =
  recursiveGroupProgram
    "review-recursive-group-unknown-member"
    [TypedDirectCallableShape]
    [Nothing]
    [[9]]

recursiveGroupDuplicateMemberProgram :: TypedProgram
recursiveGroupDuplicateMemberProgram =
  recursiveGroupProgram
    "review-recursive-group-duplicate-member"
    [TypedDirectCallableShape]
    [Just 0]
    [[0, 0]]

recursiveGroupMultipleMembershipProgram :: TypedProgram
recursiveGroupMultipleMembershipProgram =
  recursiveGroupProgram
    "review-recursive-group-multiple-membership"
    [TypedDirectCallableShape]
    [Just 0]
    [[0], [0]]

recursiveGroupMemberOrderProgram :: TypedProgram
recursiveGroupMemberOrderProgram =
  recursiveGroupProgram
    "review-recursive-group-member-order"
    [TypedDirectCallableShape, TypedDirectCallableShape]
    [Just 1, Just 0]
    [[1, 0]]

recursiveGroupOrderProgram :: TypedProgram
recursiveGroupOrderProgram =
  recursiveGroupProgram
    "review-recursive-group-order"
    [TypedDirectCallableShape, TypedDirectCallableShape]
    [Just 0, Just 1]
    [[1], [0]]

recursiveGroupMissingCycleProgram :: TypedProgram
recursiveGroupMissingCycleProgram =
  TypedProgram Nothing [moduleValue] modulePath
  where
    fixture = "review-recursive-group-missing-cycle"
    modulePath = fixtureModulePath fixture
    firstName = fixtureValueName "first"
    secondName = fixtureValueName "second"
    firstSignatureOwner = recursiveGroupOwnerAt fixture 0 "first"
    firstOwner = recursiveGroupOwnerAt fixture 1 "first"
    secondSignatureOwner = recursiveGroupOwnerAt fixture 2 "second"
    secondOwner = recursiveGroupOwnerAt fixture 3 "second"
    firstSignatureScheme = recursiveGroupScheme firstSignatureOwner TypedDirectCallableShape
    firstScheme = recursiveGroupScheme firstOwner TypedDirectCallableShape
    secondSignatureScheme = recursiveGroupScheme secondSignatureOwner TypedDirectCallableShape
    secondScheme = recursiveGroupScheme secondOwner TypedDirectCallableShape
    statements =
      [ TypedSignatureStatement firstSignatureOwner firstName span1 firstSignatureScheme,
        recursiveGroupCallable modulePath 1 firstOwner firstName firstScheme secondOwner secondName,
        TypedSignatureStatement secondSignatureOwner secondName span1 secondSignatureScheme,
        recursiveGroupCallable modulePath 3 secondOwner secondName secondScheme firstOwner firstName
      ]
    moduleValue =
      TypedModule modulePath relativeSource [] [] emptyInterface [] statements unitInfo

recursiveGroupSpuriousCycleProgram :: TypedProgram
recursiveGroupSpuriousCycleProgram =
  recursiveGroupProgram
    "review-recursive-group-spurious-cycle"
    [TypedDirectCallableShape]
    [Nothing]
    [[0]]

recursiveGroupMixedShapesProgram :: TypedProgram
recursiveGroupMixedShapesProgram =
  recursiveGroupProgram
    "review-recursive-group-mixed-shapes"
    [TypedDirectCallableShape, TypedClosureCallableShape]
    [Just 0, Just 1]
    [[0, 1]]

recursiveGroupProgram :: Text -> [TypedCallableShape] -> [Maybe Int] -> [[Int]] -> TypedProgram
recursiveGroupProgram fixture shapes dependencies groupIndices =
  TypedProgram Nothing [moduleValue] modulePath
  where
    modulePath = fixtureModulePath fixture
    names = [fixtureValueName ("function" <> Text.pack (show index)) | index <- [0 .. length shapes - 1]]
    owners = [binder modulePath [index] name | (index, name) <- zip [0 ..] names]
    schemes = zipWith recursiveGroupScheme owners shapes
    statements =
      [ case dependency of
          Nothing -> recursiveGroupIdentityCallable modulePath index owner name scheme
          Just dependencyIndex ->
            recursiveGroupCallable modulePath index owner name scheme (owners !! dependencyIndex) (names !! dependencyIndex)
      | (index, owner, name, scheme, dependency) <- zip5 [0 ..] owners names schemes dependencies
      ]
    recursiveGroups =
      [TypedRecursiveGroup [recursiveGroupOwner fixture index | index <- indices] | indices <- groupIndices]
    moduleValue =
      TypedModule modulePath relativeSource [] [] emptyInterface recursiveGroups statements unitInfo

recursiveGroupScheme :: TypedBinderId -> TypedCallableShape -> TypedScheme
recursiveGroupScheme owner shape =
  TypedScheme owner [] [] [] boolToBoolType boolToBoolRecipe (Just shape)

recursiveGroupCallable :: [Text] -> Int -> TypedBinderId -> TypedCoreName -> TypedScheme -> TypedBinderId -> TypedCoreName -> TypedStatement
recursiveGroupCallable modulePath statementIndex owner name scheme dependencyOwner dependencyName =
  TypedLetStatement owner name span1 scheme expression
  where
    argumentName = fixtureValueName ("argument" <> Text.pack (show statementIndex))
    argumentOwner = binder modulePath [statementIndex, 0] argumentName
    expression =
      TypedLambdaExpr
        boolToBoolInfo
        argumentOwner
        argumentName
        ( TypedApplyExpr
            boolInfo
            (fixtureBoundVariableExpr dependencyOwner boolToBoolInfo dependencyName)
            (fixtureBoundVariableExpr argumentOwner boolInfo argumentName)
        )

recursiveGroupIdentityCallable :: [Text] -> Int -> TypedBinderId -> TypedCoreName -> TypedScheme -> TypedStatement
recursiveGroupIdentityCallable modulePath statementIndex owner name scheme =
  TypedLetStatement owner name span1 scheme expression
  where
    argumentName = fixtureValueName ("argument" <> Text.pack (show statementIndex))
    argumentOwner = binder modulePath [statementIndex, 0] argumentName
    expression =
      TypedLambdaExpr
        boolToBoolInfo
        argumentOwner
        argumentName
        (fixtureBoundVariableExpr argumentOwner boolInfo argumentName)

recursiveGroupOwner :: Text -> Int -> TypedBinderId
recursiveGroupOwner fixture statementIndex =
  recursiveGroupOwnerAt fixture statementIndex ("function" <> Text.pack (show statementIndex))

recursiveGroupOwnerAt :: Text -> Int -> Text -> TypedBinderId
recursiveGroupOwnerAt fixture statementIndex name =
  binder (fixtureModulePath fixture) [statementIndex] (fixtureValueName name)

malformedLiteralConstraintBoundsProgram :: TypedProgram
malformedLiteralConstraintBoundsProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface unitInfo modulePath
  where
    fixture = "review-malformed-literal-constraint-bounds"
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    signature statementIndex suffix lower upper =
      let valueName = resolved TypedCurrentModule TypedValueNamespace suffix
          owner = binder modulePath [statementIndex] valueName
          scheme =
            fixtureScheme
              owner
              [parameter]
              []
              [TypedNumericPrimitiveConstraint (TypedIntegralLiteralNumericConstraint lower upper) parameterType]
              TypedBoolType
              TypedBoolRecipe
       in TypedSignatureStatement owner valueName span1 scheme
    statements =
      [ signature 0 "reversed" "10" "2",
        signature 1 "nonDecimal" "zero" "10"
      ]

evidenceSelectionOrderProgram :: TypedProgram
evidenceSelectionOrderProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-evidence-selection-order"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "constrained"
    owner = binder modulePath [0] valueName
    firstParameter = TypedEvidenceParameterId 0
    secondParameter = TypedEvidenceParameterId 1
    firstConstraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    secondConstraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedCharType
    scheme =
      fixtureScheme
        owner
        []
        [ TypedEvidenceParameter firstParameter firstConstraint,
          TypedEvidenceParameter secondParameter secondConstraint
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    selection parameter constraint target =
      TypedSelectedEvidence
        ( TypedEvidenceUse
            (Just (TypedEvidenceParameterRef owner parameter))
            constraint
            (TypedImplId ["Prelude"] capabilityName [target])
            Nothing
        )
    expression =
      fixtureVariableExpr
        ( TypedNodeInfo
            TypedBoolType
            TypedBoolRecipe
            [TypedInstantiation owner [] Nothing]
            [ selection secondParameter secondConstraint TypedCharType,
              selection firstParameter firstConstraint TypedBoolType
            ]
        )
        valueName
    statements =
      [ TypedLetStatement owner valueName span1 scheme trueExpr,
        expressionStatement 1 expression
      ]

privateCapabilityMetadataVisibilityProgram :: TypedProgram
privateCapabilityMetadataVisibilityProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "PrivateCapabilityMetadata")
    entryPath = (fixtureModulePath "review-private-capability-metadata-visibility")
    parameter = TypedTypeParameterId 0
    libraryCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "PrivateEq"
    libraryClass =
      TypedClassDeclaration span1 libraryCapabilityName [parameter] []
    valueName = resolved TypedCurrentModule TypedValueNamespace "constrained"
    valueOwner = binder libraryPath [1] valueName
    valueScheme =
      fixtureScheme
        valueOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint libraryCapabilityName Nothing TypedBoolType)
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/PrivateCapabilityMetadata.jz")
        []
        [TypedModuleExport TypedValueNamespace "constrained"]
        ( TypedModuleInterface
            [TypedValueInterface valueName valueScheme]
            []
            [TypedClassInterface libraryClass]
            []
        )
        [ TypedClassStatement libraryClass,
          TypedLetStatement valueOwner valueName span1 valueScheme trueExpr
        ]
        unitInfo
    localCapabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "PrivateEq"
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["constrained"])]
        []
        emptyInterface
        [ TypedClassStatement
            (TypedClassDeclaration span1 localCapabilityName [parameter] []),
          expressionStatement 1 trueExpr
        ]
        boolInfo

moduleQualifiedMethodKeyProgram :: TypedProgram
moduleQualifiedMethodKeyProgram =
  importedModuleQualifiedMethodKeyProgram
    "review-module-qualified-method-key"
    "Lib::Api::Make::make"

forgedModuleQualifiedMethodKeyProgram :: TypedProgram
forgedModuleQualifiedMethodKeyProgram =
  importedModuleQualifiedMethodKeyProgram
    "review-forged-module-qualified-method-key"
    "Other::Make::make"

importedModuleQualifiedMethodKeyProgram :: Text -> Text -> TypedProgram
importedModuleQualifiedMethodKeyProgram fixture qualifiedMethod =
  TypedProgram Nothing [providerModule, entryModule] entryPath
  where
    providerPath = ["Lib", "Api"]
    entryPath = fixtureModulePath fixture
    capabilityIdentifier = "Make"
    parameter = TypedTypeParameterId 0
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace capabilityIdentifier
    methodName = resolved TypedCurrentModule TypedValueNamespace "make"
    methodOwner = binder providerPath [0, 0] methodName
    methodScheme = monoScheme methodOwner
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    implId = TypedImplId providerPath capabilityName [TypedBoolType]
    methodDefinition =
      TypedMethodDefinition
        (TypedMethodId implId "make")
        (binder providerPath [1, 0] methodName)
        methodName
        span1
        trueExpr
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Lib/Api.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace capabilityIdentifier]
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface classDeclaration]
            [TypedImplInterface implId]
        )
        [ TypedClassStatement classDeclaration,
          TypedImplStatement (TypedImplDeclaration span1 implId [methodDefinition])
        ]
        unitInfo
    importedCapabilityName =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        capabilityIdentifier
    importedImplId =
      TypedImplId providerPath importedCapabilityName [TypedBoolType]
    evidenceUse =
      TypedEvidenceUse
        Nothing
        (TypedCapabilityConstraint importedCapabilityName (Just qualifiedMethod) TypedBoolType)
        importedImplId
        (Just (TypedMethodId importedImplId "make"))
    expression =
      fixtureVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBuiltinName qualifiedMethod)
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 providerPath Nothing Nothing]
        []
        emptyInterface
        [expressionStatement 1 expression]
        (expressionInfoForFixture expression)

importedDataDependencyProgram :: TypedProgram
importedDataDependencyProgram =
  TypedProgram Nothing [providerModule, facadeModule, entryModule] entryPath
  where
    providerPath = (fixtureLibraryPath "ImportedDataProvider")
    facadePath = (fixtureLibraryPath "ImportedDataFacade")
    entryPath = (fixtureModulePath "review-imported-data-dependency")
    providerBoxName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    importedBoxName =
      resolved (TypedImportedModule providerPath) TypedTypeNamespace "Box"
    importedBoxConstructor =
      resolved (TypedImportedModule providerPath) TypedConstructorNamespace "Box"
    boxDeclaration =
      dataDeclarationWithNullaryConstructor
        providerPath
        [0, 0]
        providerBoxName
        []
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/ImportedDataProvider.jz")
        []
        [ TypedModuleExport TypedTypeNamespace "Box",
          TypedModuleExport TypedConstructorNamespace "Box"
        ]
        (TypedModuleInterface [] [TypedDataInterface boxDeclaration] [] [])
        [TypedDataStatement boxDeclaration]
        unitInfo
    valueName = resolved TypedCurrentModule TypedValueNamespace "published"
    importedValueName =
      resolved (TypedImportedModule facadePath) TypedValueNamespace "published"
    valueOwner = binder facadePath [0] valueName
    boxType = TypedDataType importedBoxName []
    boxRecipe = TypedManagedVariantRecipe importedBoxName []
    valueScheme =
      fixtureScheme valueOwner [] [] [] boxType boxRecipe
    facadeModule =
      typedModule
        facadePath
        (TypedSourcePath "src/Library/ImportedDataFacade.jz")
        [TypedResolvedImport span1 providerPath Nothing (Just ["Box"])]
        [TypedModuleExport TypedValueNamespace "published"]
        (TypedModuleInterface [TypedValueInterface valueName valueScheme] [] [] [])
        [ TypedLetStatement
            valueOwner
            valueName
            span1
            valueScheme
            (fixtureBoundVariableExpr (binder providerPath [0, 0] (resolved TypedCurrentModule TypedConstructorNamespace "Box")) (info boxType boxRecipe) importedBoxConstructor)
        ]
        unitInfo
    entryInfo = info boxType boxRecipe
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 facadePath Nothing (Just ["published"])]
        []
        emptyInterface
        [expressionStatement 1 (fixtureBoundVariableExpr valueOwner entryInfo importedValueName)]
        entryInfo

transitiveDataContractDependencyProgram :: TypedProgram
transitiveDataContractDependencyProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "TransitiveDataContract")
    entryPath = (fixtureModulePath "review-transitive-data-contract-dependency")
    hiddenName = resolved TypedCurrentModule TypedTypeNamespace "Hidden"
    boxName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    boxConstructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Box"
    hiddenType = TypedDataType hiddenName []
    boxType = TypedDataType boxName []
    hiddenDeclaration =
      dataDeclarationWithNullaryConstructor libraryPath [0, 0] hiddenName []
    boxDeclaration =
      TypedDataDeclaration
        span1
        boxName
        []
        [ TypedConstructorDeclaration
            (binder libraryPath [1, 0] boxConstructorName)
            boxConstructorName
            [hiddenType]
            [TypedManagedVariantRecipe hiddenName []]
        ]
    valueName = resolved TypedCurrentModule TypedValueNamespace "published"
    importedValueName =
      resolved (TypedImportedModule libraryPath) TypedValueNamespace "published"
    valueOwner = binder libraryPath [2] valueName
    parameter = TypedTypeParameterId 0
    valueScheme =
      fixtureScheme
        valueOwner
        [parameter]
        []
        [TypedStrictEqualityPrimitiveConstraint boxType]
        TypedBoolType
        TypedBoolRecipe
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/TransitiveDataContract.jz")
        []
        [TypedModuleExport TypedValueNamespace "published"]
        ( TypedModuleInterface
            [TypedValueInterface valueName valueScheme]
            [ TypedDataInterface hiddenDeclaration,
              TypedDataInterface boxDeclaration
            ]
            []
            []
        )
        [ TypedDataStatement hiddenDeclaration,
          TypedDataStatement boxDeclaration,
          TypedLetStatement valueOwner valueName span1 valueScheme trueExpr
        ]
        unitInfo
    instantiation =
      TypedInstantiation
        valueOwner
        [TypedTypeArgument parameter TypedBoolType]
        Nothing
    entryInfo =
      TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["published"])]
        []
        emptyInterface
        [expressionStatement 1 (fixtureVariableExpr entryInfo importedValueName)]
        entryInfo

importedCapabilityFacadePath :: [Text]
importedCapabilityFacadePath = (fixtureLibraryPath "ImportedCapabilityFacade")

importedCapabilityDependencyProgram :: TypedProgram
importedCapabilityDependencyProgram =
  TypedProgram Nothing [providerModule, facadeModule] importedCapabilityFacadePath
  where
    providerPath = (fixtureLibraryPath "ImportedCapabilityProvider")
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "ForeignEq"
    parameter = TypedTypeParameterId 0
    capability =
      TypedClassDeclaration span1 capabilityName [parameter] []
    providerModule =
      typedModule
        providerPath
        (TypedSourcePath "src/Library/ImportedCapabilityProvider.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "ForeignEq"]
        (TypedModuleInterface [] [] [TypedClassInterface capability] [])
        [TypedClassStatement capability]
        unitInfo
    valueName = resolved TypedCurrentModule TypedValueNamespace "published"
    valueOwner = binder importedCapabilityFacadePath [0] valueName
    importedCapabilityName =
      resolved
        (TypedImportedModule providerPath)
        TypedCapabilityNamespace
        "ForeignEq"
    valueScheme =
      fixtureScheme
        valueOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint importedCapabilityName Nothing TypedBoolType)
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    facadeModule =
      typedModule
        importedCapabilityFacadePath
        (TypedSourcePath "src/Library/ImportedCapabilityFacade.jz")
        [TypedResolvedImport span1 providerPath Nothing (Just ["ForeignEq"])]
        [TypedModuleExport TypedValueNamespace "published"]
        (TypedModuleInterface [TypedValueInterface valueName valueScheme] [] [] [])
        [TypedLetStatement valueOwner valueName span1 valueScheme trueExpr]
        unitInfo

metadataOnlyImportedCapabilityName :: TypedCoreName
metadataOnlyImportedCapabilityName =
  resolved
    (TypedImportedModule (fixtureLibraryPath "MetadataOnlyImpl"))
    TypedCapabilityNamespace
    "PrivateEq"

metadataOnlyImportedImpl :: TypedImplId
metadataOnlyImportedImpl =
  TypedImplId
    (fixtureLibraryPath "MetadataOnlyImpl")
    metadataOnlyImportedCapabilityName
    [TypedBoolType]

metadataOnlyImplVisibilityProgram :: TypedProgram
metadataOnlyImplVisibilityProgram =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "MetadataOnlyImpl")
    entryPath = (fixtureModulePath "review-metadata-only-impl-visibility")
    parameter = TypedTypeParameterId 0
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "PrivateEq"
    capability =
      TypedClassDeclaration span1 capabilityName [parameter] []
    localImpl =
      TypedImplId libraryPath capabilityName [TypedBoolType]
    valueName = resolved TypedCurrentModule TypedValueNamespace "constrained"
    valueOwner = binder libraryPath [2] valueName
    constraint =
      TypedCapabilityConstraint capabilityName Nothing TypedBoolType
    importedConstraint =
      TypedCapabilityConstraint metadataOnlyImportedCapabilityName Nothing TypedBoolType
    valueScheme =
      fixtureScheme
        valueOwner
        []
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/MetadataOnlyImpl.jz")
        []
        [TypedModuleExport TypedValueNamespace "constrained"]
        ( TypedModuleInterface
            [TypedValueInterface valueName valueScheme]
            []
            [TypedClassInterface capability]
            [TypedImplInterface localImpl]
        )
        [ TypedClassStatement capability,
          TypedImplStatement (TypedImplDeclaration span1 localImpl []),
          TypedLetStatement valueOwner valueName span1 valueScheme trueExpr
        ]
        unitInfo
    evidenceUse =
      TypedEvidenceUse Nothing importedConstraint metadataOnlyImportedImpl Nothing
    expression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
        (TypedBooleanLiteral True)
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["constrained"])]
        []
        emptyInterface
        [expressionStatement 1 expression]
        boolInfo

patternExpressionMetadataProgram :: TypedProgram
patternExpressionMetadataProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-pattern-expression-metadata"
    modulePath = (fixtureModulePath fixture)
    genericName = fixtureValueName "generic"
    genericOwner = binder modulePath [0] genericName
    parameter = TypedTypeParameterId 0
    genericScheme =
      fixtureScheme genericOwner [parameter] [] [] TypedBoolType TypedBoolRecipe
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "PatternMarker"
    capability =
      TypedClassDeclaration span1 capabilityName [parameter] []
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    constraint =
      TypedCapabilityConstraint capabilityName Nothing TypedBoolType
    patternInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [TypedInstantiation genericOwner [TypedTypeArgument parameter TypedBoolType] Nothing]
        [TypedSelectedEvidence (TypedEvidenceUse Nothing constraint implId Nothing)]
    expression =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm (TypedWildcardPattern patternInfo) Nothing trueExpr]
    statements =
      [ TypedLetStatement genericOwner genericName span1 genericScheme trueExpr,
        TypedClassStatement capability,
        TypedImplStatement (TypedImplDeclaration span1 implId []),
        expressionStatement 4 expression
      ]

phantomDataEqualityProgram :: TypedProgram
phantomDataEqualityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-phantom-data-equality"
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Phantom"
    constructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Phantom"
    parameter = TypedTypeParameterId 0
    declaration =
      TypedDataDeclaration
        span1
        dataName
        [parameter]
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] constructorName)
            constructorName
            []
            []
        ]
    valueName = fixtureValueName "phantomEquality"
    valueOwner = binder modulePath [1] valueName
    phantomFunctionType = TypedDataType dataName [boolToBoolType]
    scheme =
      fixtureScheme
        valueOwner
        []
        []
        [TypedStrictEqualityPrimitiveConstraint phantomFunctionType]
        TypedBoolType
        TypedBoolRecipe
    statements =
      [ TypedDataStatement declaration,
        TypedLetStatement valueOwner valueName span1 scheme trueExpr
      ]

sameScopeValueRebindingProgram :: TypedProgram
sameScopeValueRebindingProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
  where
    fixture = "review-same-scope-value-rebinding"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "item"
    firstSignatureOwner = binder modulePath [0] valueName
    firstOwner = binder modulePath [1] valueName
    secondSignatureOwner = binder modulePath [2] valueName
    secondOwner = binder modulePath [3] valueName
    firstSignatureScheme =
      fixtureScheme firstSignatureOwner [] [] [] TypedBoolType TypedBoolRecipe
    firstScheme =
      fixtureScheme firstOwner [] [] [] TypedBoolType TypedBoolRecipe
    secondSignatureScheme =
      fixtureScheme secondSignatureOwner [] [] [] TypedTextType TypedManagedTextRecipe
    secondScheme =
      fixtureScheme secondOwner [] [] [] TypedTextType TypedManagedTextRecipe
    statements =
      [ TypedSignatureStatement firstSignatureOwner valueName span1 firstSignatureScheme,
        TypedLetStatement firstOwner valueName span1 firstScheme trueExpr,
        TypedSignatureStatement secondSignatureOwner valueName span1 secondSignatureScheme,
        TypedLetStatement
          secondOwner
          valueName
          span1
          secondScheme
          (TypedLiteralExpr textInfo (TypedTextLiteral "latest")),
        expressionStatement 4 (fixtureBoundVariableExpr secondOwner textInfo valueName)
      ]

nearestPriorBindingDependencyProgram :: TypedProgram
nearestPriorBindingDependencyProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-nearest-prior-binding-dependency"
    modulePath = fixtureModulePath fixture
    repeatedName = fixtureValueName "item"
    linkName = fixtureValueName "link"
    resultName = fixtureValueName "result"
    firstOwner = binder modulePath [0] repeatedName
    linkOwner = binder modulePath [1] linkName
    secondOwner = binder modulePath [2] repeatedName
    resultOwner = binder modulePath [3] resultName
    statements =
      [ TypedLetStatement firstOwner repeatedName span1 (monoScheme firstOwner) trueExpr,
        TypedLetStatement
          linkOwner
          linkName
          span1
          (monoScheme linkOwner)
          (fixtureBoundVariableExpr firstOwner boolInfo repeatedName),
        TypedLetStatement
          secondOwner
          repeatedName
          span1
          (monoScheme secondOwner)
          (fixtureBoundVariableExpr linkOwner boolInfo linkName),
        TypedLetStatement
          resultOwner
          resultName
          span1
          (monoScheme resultOwner)
          (fixtureBoundVariableExpr secondOwner boolInfo repeatedName),
        expressionStatement 4 (fixtureBoundVariableExpr resultOwner boolInfo resultName)
      ]

sourceOrderedRecursiveVisibilityProgram :: TypedProgram
sourceOrderedRecursiveVisibilityProgram =
  TypedProgram Nothing [moduleValue] modulePath
  where
    fixture = "review-source-ordered-recursive-visibility"
    modulePath = fixtureModulePath fixture
    firstName = fixtureValueName "first"
    bridgeName = fixtureValueName "bridge"
    middleName = fixtureValueName "middle"
    tailName = fixtureValueName "tail"
    firstOwner = binder modulePath [0] firstName
    bridgeOwner = binder modulePath [2] bridgeName
    middleOwner = binder modulePath [4] middleName
    tailOwner = binder modulePath [6] tailName
    moduleValue =
      TypedModule
        modulePath
        relativeSource
        []
        []
        emptyInterface
        [TypedRecursiveGroup [firstOwner, bridgeOwner, middleOwner, tailOwner]]
        statements
        boolInfo
    recursiveBinding statementIndex owner name peerOwner peerName =
      let argumentName = fixtureValueName ("argument" <> Text.pack (show statementIndex))
          argumentOwner = binder modulePath [statementIndex, 0] argumentName
       in TypedLetStatement
            owner
            name
            span1
            (fixtureScheme owner [] [] [] boolToBoolType boolToBoolRecipe)
            ( TypedLambdaExpr
                boolToBoolInfo
                argumentOwner
                argumentName
                ( TypedApplyExpr
                    boolInfo
                    (fixtureBoundVariableExpr peerOwner boolToBoolInfo peerName)
                    (fixtureBoundVariableExpr argumentOwner boolInfo argumentName)
                )
            )
    unseenTail = fixtureVariableExpr boolToBoolInfo tailName
    statements =
      [ recursiveBinding 0 firstOwner firstName tailOwner tailName,
        expressionStatement 1 unseenTail,
        recursiveBinding 2 bridgeOwner bridgeName firstOwner firstName,
        expressionStatement 3 unseenTail,
        recursiveBinding 4 middleOwner middleName bridgeOwner bridgeName,
        expressionStatement 5 unseenTail,
        recursiveBinding 6 tailOwner tailName middleOwner middleName,
        expressionStatement
          7
          ( TypedApplyExpr
              boolInfo
              (fixtureBoundVariableExpr middleOwner boolToBoolInfo middleName)
              trueExpr
          )
      ]

forwardModuleReferenceProgram :: TypedProgram
forwardModuleReferenceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-forward-module-reference"
    modulePath = (fixtureModulePath fixture)
    firstName = fixtureValueName "first"
    laterName = fixtureValueName "later"
    firstOwner = binder modulePath [0] firstName
    laterOwner = binder modulePath [1] laterName
    statements =
      [ TypedLetStatement
          firstOwner
          firstName
          span1
          (monoScheme firstOwner)
          (fixtureVariableExpr boolInfo laterName),
        TypedLetStatement laterOwner laterName span1 (monoScheme laterOwner) trueExpr,
        expressionStatement 3 (fixtureBoundVariableExpr firstOwner boolInfo firstName)
      ]

forwardSignedVisibilityPrograms :: [(Text, TypedProgram)]
forwardSignedVisibilityPrograms =
  [ ( "forward-signed-function-visibility",
      forwardVisibilityProgram "forward-signed-function-visibility" True True
    ),
    ( "forward-signed-scalar-invisibility",
      forwardVisibilityProgram "forward-signed-scalar-invisibility" True False
    ),
    ( "forward-unsigned-function-invisibility",
      forwardVisibilityProgram "forward-unsigned-function-invisibility" False True
    ),
    ( "forward-signed-function-hidden-from-unsigned-caller",
      unsignedForwardCallerProgram
    ),
    ( "forward-signed-function-hidden-from-scalar-expression",
      scalarForwardReferenceProgram
    )
  ]

forwardVisibilityProgram :: Text -> Bool -> Bool -> TypedProgram
forwardVisibilityProgram fixture laterIsSigned laterIsFunction =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    modulePath = fixtureModulePath fixture
    firstName = fixtureValueName "first"
    laterName = fixtureValueName "later"
    firstSignatureOwner = binder modulePath [0] firstName
    firstOwner = binder modulePath [1] firstName
    firstArgumentName = fixtureValueName "firstArgument"
    firstArgumentOwner = binder modulePath [1, 0] firstArgumentName
    firstSignatureScheme =
      fixtureScheme firstSignatureOwner [] [] [] boolToBoolType boolToBoolRecipe
    firstScheme =
      fixtureScheme firstOwner [] [] [] boolToBoolType boolToBoolRecipe
    firstBody
      | laterIsFunction =
          TypedApplyExpr
            boolInfo
            laterFunctionReference
            (fixtureBoundVariableExpr firstArgumentOwner boolInfo firstArgumentName)
      | otherwise = fixtureVariableExpr boolInfo laterName
    laterFunctionReference
      | laterIsSigned = fixtureBoundVariableExpr laterOwner boolToBoolInfo laterName
      | otherwise = fixtureVariableExpr boolToBoolInfo laterName
    firstExpression =
      TypedLambdaExpr
        boolToBoolInfo
        firstArgumentOwner
        firstArgumentName
        firstBody
    laterStatementIndex = if laterIsSigned then 3 else 2
    laterOwner = binder modulePath [laterStatementIndex] laterName
    laterScheme
      | laterIsFunction =
          fixtureScheme laterOwner [] [] [] boolToBoolType boolToBoolRecipe
      | otherwise = monoScheme laterOwner
    laterArgumentName = fixtureValueName "laterArgument"
    laterArgumentOwner = binder modulePath [laterStatementIndex, 0] laterArgumentName
    laterExpression
      | laterIsFunction =
          TypedLambdaExpr
            boolToBoolInfo
            laterArgumentOwner
            laterArgumentName
            (fixtureBoundVariableExpr laterArgumentOwner boolInfo laterArgumentName)
      | otherwise = trueExpr
    laterSignature =
      let signatureOwner = binder modulePath [2] laterName
          signatureScheme
            | laterIsFunction =
                fixtureScheme signatureOwner [] [] [] boolToBoolType boolToBoolRecipe
            | otherwise = monoScheme signatureOwner
       in TypedSignatureStatement signatureOwner laterName span1 signatureScheme
    terminalStatementIndex = laterStatementIndex + 1
    terminalExpression =
      TypedApplyExpr
        boolInfo
        (fixtureBoundVariableExpr firstOwner boolToBoolInfo firstName)
        trueExpr
    statements =
      [ TypedSignatureStatement firstSignatureOwner firstName span1 firstSignatureScheme,
        TypedLetStatement firstOwner firstName span1 firstScheme firstExpression
      ]
        <> [laterSignature | laterIsSigned]
        <> [ TypedLetStatement laterOwner laterName span1 laterScheme laterExpression,
             expressionStatement terminalStatementIndex terminalExpression
           ]

unsignedForwardCallerProgram :: TypedProgram
unsignedForwardCallerProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "forward-signed-function-hidden-from-unsigned-caller"
    modulePath = fixtureModulePath fixture
    firstName = fixtureValueName "first"
    laterName = fixtureValueName "later"
    firstOwner = binder modulePath [0] firstName
    firstArgumentName = fixtureValueName "firstArgument"
    firstArgumentOwner = binder modulePath [0, 0] firstArgumentName
    firstScheme = fixtureScheme firstOwner [] [] [] boolToBoolType boolToBoolRecipe
    firstExpression =
      TypedLambdaExpr
        boolToBoolInfo
        firstArgumentOwner
        firstArgumentName
        ( TypedApplyExpr
            boolInfo
            (fixtureVariableExpr boolToBoolInfo laterName)
            (fixtureBoundVariableExpr firstArgumentOwner boolInfo firstArgumentName)
        )
    laterSignatureOwner = binder modulePath [1] laterName
    laterOwner = binder modulePath [2] laterName
    laterArgumentName = fixtureValueName "laterArgument"
    laterArgumentOwner = binder modulePath [2, 0] laterArgumentName
    laterScheme = fixtureScheme laterOwner [] [] [] boolToBoolType boolToBoolRecipe
    laterExpression =
      TypedLambdaExpr
        boolToBoolInfo
        laterArgumentOwner
        laterArgumentName
        (fixtureBoundVariableExpr laterArgumentOwner boolInfo laterArgumentName)
    statements =
      [ TypedLetStatement firstOwner firstName span1 firstScheme firstExpression,
        TypedSignatureStatement
          laterSignatureOwner
          laterName
          span1
          (fixtureScheme laterSignatureOwner [] [] [] boolToBoolType boolToBoolRecipe),
        TypedLetStatement laterOwner laterName span1 laterScheme laterExpression,
        expressionStatement
          3
          ( TypedApplyExpr
              boolInfo
              (fixtureBoundVariableExpr firstOwner boolToBoolInfo firstName)
              trueExpr
          )
      ]

scalarForwardReferenceProgram :: TypedProgram
scalarForwardReferenceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "forward-signed-function-hidden-from-scalar-expression"
    modulePath = fixtureModulePath fixture
    laterName = fixtureValueName "later"
    laterSignatureOwner = binder modulePath [1] laterName
    laterOwner = binder modulePath [2] laterName
    laterArgumentName = fixtureValueName "laterArgument"
    laterArgumentOwner = binder modulePath [2, 0] laterArgumentName
    laterScheme = fixtureScheme laterOwner [] [] [] boolToBoolType boolToBoolRecipe
    laterExpression =
      TypedLambdaExpr
        boolToBoolInfo
        laterArgumentOwner
        laterArgumentName
        (fixtureBoundVariableExpr laterArgumentOwner boolInfo laterArgumentName)
    statements =
      [ expressionStatement
          1
          ( TypedApplyExpr
              boolInfo
              (fixtureVariableExpr boolToBoolInfo laterName)
              trueExpr
          ),
        TypedSignatureStatement
          laterSignatureOwner
          laterName
          span1
          (fixtureScheme laterSignatureOwner [] [] [] boolToBoolType boolToBoolRecipe),
        TypedLetStatement laterOwner laterName span1 laterScheme laterExpression,
        expressionStatement 3 trueExpr
      ]

nestedForwardSignedFunctionProgram :: TypedProgram
nestedForwardSignedFunctionProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-forward-signed-function-invisibility"
    modulePath = fixtureModulePath fixture
    firstName = fixtureValueName "first"
    laterName = fixtureValueName "later"
    firstSignatureOwner = binder modulePath [0, 0, 0] firstName
    firstOwner = binder modulePath [0, 0, 1] firstName
    firstArgumentName = fixtureValueName "firstArgument"
    firstArgumentOwner = binder modulePath [0, 0, 1, 0] firstArgumentName
    firstScheme = fixtureScheme firstOwner [] [] [] boolToBoolType boolToBoolRecipe
    firstExpression =
      TypedLambdaExpr
        boolToBoolInfo
        firstArgumentOwner
        firstArgumentName
        ( TypedApplyExpr
            boolInfo
            (fixtureVariableExpr boolToBoolInfo laterName)
            (fixtureBoundVariableExpr firstArgumentOwner boolInfo firstArgumentName)
        )
    laterSignatureOwner = binder modulePath [0, 0, 2] laterName
    laterOwner = binder modulePath [0, 0, 3] laterName
    laterArgumentName = fixtureValueName "laterArgument"
    laterArgumentOwner = binder modulePath [0, 0, 3, 0] laterArgumentName
    laterScheme = fixtureScheme laterOwner [] [] [] boolToBoolType boolToBoolRecipe
    laterExpression =
      TypedLambdaExpr
        boolToBoolInfo
        laterArgumentOwner
        laterArgumentName
        (fixtureBoundVariableExpr laterArgumentOwner boolInfo laterArgumentName)
    block =
      TypedBlockExpr
        boolInfo
        [ TypedSignatureStatement
            firstSignatureOwner
            firstName
            span1
            (fixtureScheme firstSignatureOwner [] [] [] boolToBoolType boolToBoolRecipe),
          TypedLetStatement firstOwner firstName span1 firstScheme firstExpression,
          TypedSignatureStatement
            laterSignatureOwner
            laterName
            span1
            (fixtureScheme laterSignatureOwner [] [] [] boolToBoolType boolToBoolRecipe),
          TypedLetStatement laterOwner laterName span1 laterScheme laterExpression,
          expressionStatement 2 (TypedApplyExpr boolInfo (fixtureBoundVariableExpr firstOwner boolToBoolInfo firstName) trueExpr)
        ]

missingPolymorphicInstantiationOwner :: TypedBinderId
missingPolymorphicInstantiationOwner =
  fixtureBinder
    "review-missing-polymorphic-instantiation"
    0
    (fixtureValueName "identity")

missingPolymorphicInstantiationProgram :: TypedProgram
missingPolymorphicInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-missing-polymorphic-instantiation"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "identity"
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    scheme =
      fixtureClosureScheme
        missingPolymorphicInstantiationOwner
        [parameter]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    statements =
      [ TypedLetStatement
          missingPolymorphicInstantiationOwner
          valueName
          span1
          scheme
          (polymorphicIdentityExpression modulePath [0] parameter),
        expressionStatement 1 (fixtureBoundVariableExpr missingPolymorphicInstantiationOwner boolToBoolInfo valueName)
      ]

unsupportedEqualityDataName :: TypedCoreName
unsupportedEqualityDataName = resolved TypedCurrentModule TypedTypeNamespace "CallableBox"

unsupportedEqualityDataType :: TypedType
unsupportedEqualityDataType = TypedDataType unsupportedEqualityDataName []

unsupportedStrictEqualityConstraintProgram :: TypedProgram
unsupportedStrictEqualityConstraintProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-unsupported-strict-equality-constraint"
    modulePath = (fixtureModulePath fixture)
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "CallableBox"
    dataDeclaration =
      TypedDataDeclaration
        span1
        unsupportedEqualityDataName
        []
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] constructorName)
            constructorName
            [boolToBoolType]
            [boolToBoolRecipe]
        ]
    functionName = fixtureValueName "functionEquality"
    functionOwner = binder modulePath [1] functionName
    dataName = fixtureValueName "dataEquality"
    dataOwner = binder modulePath [2] dataName
    constrained owner target =
      TypedSignatureStatement
        owner
        (case owner of TypedBinderId (_, _, name) -> name)
        span1
        (fixtureScheme owner [] [] [TypedStrictEqualityPrimitiveConstraint target] TypedBoolType TypedBoolRecipe)
    statements =
      [ TypedDataStatement dataDeclaration,
        constrained functionOwner boolToBoolType,
        constrained dataOwner unsupportedEqualityDataType
      ]

uncheckedSpecialNameProgram :: TypedProgram
uncheckedSpecialNameProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface builtinMapInfo (fixtureModulePath fixture)
  where
    fixture = "review-unchecked-special-name"
    statements =
      [ expressionStatement 1 (fixtureVariableExpr boolInfo (TypedBuiltinName "doesNotExist")),
        expressionStatement 2 (fixtureVariableExpr boolInfo (TypedGeneratedName TypedOperatorSectionFunction)),
        expressionStatement 3 (fixtureVariableExpr builtinMapInfo (TypedBuiltinName "map"))
      ]

classMethodExportProgram :: TypedProgram
classMethodExportProgram =
  targetIndependentClassMethodImportProgramWith
    "review-class-method-export"
    True

missingTargetIndependentClassMethodDispatchProgram :: TypedProgram
missingTargetIndependentClassMethodDispatchProgram =
  targetIndependentClassMethodImportProgramWith
    "review-missing-target-independent-class-method-dispatch"
    False

targetIndependentClassMethodImportProgramWith :: Text -> Bool -> TypedProgram
targetIndependentClassMethodImportProgramWith fixture includeEvidence =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "ClassMethodExport")
    entryPath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    className = resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    methodName = resolved TypedCurrentModule TypedValueNamespace "render"
    methodOwner = binder libraryPath [0, 0] methodName
    methodScheme = fixtureClosureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    declaration = TypedClassDeclaration span1 className [parameter] [TypedMethodSignature methodName span1 methodScheme]
    localImplId = TypedImplId libraryPath className [TypedBoolType]
    methodDefinition = fixtureImplMethod libraryPath [1, 0] localImplId "render"
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/ClassMethodExport.jz")
        []
        [ TypedModuleExport TypedCapabilityNamespace "Render",
          TypedModuleExport TypedValueNamespace "render"
        ]
        (TypedModuleInterface [] [] [TypedClassInterface declaration] [TypedImplInterface localImplId])
        [ TypedClassStatement declaration,
          TypedImplStatement (TypedImplDeclaration span1 localImplId [methodDefinition])
        ]
        boolInfo
    importedMethod = resolved (TypedImportedModule libraryPath) TypedValueNamespace "render"
    importedClassName =
      resolved
        (TypedImportedModule libraryPath)
        TypedCapabilityNamespace
        "Render"
    importedImplId =
      TypedImplId libraryPath importedClassName [TypedBoolType]
    constraint =
      TypedCapabilityConstraint
        importedClassName
        (Just (Text.intercalate "::" (libraryPath <> ["Render", "render"])))
        TypedBoolType
    selectedEvidence =
      TypedSelectedEvidence
        ( TypedEvidenceUse
            ( Just
                ( TypedEvidenceParameterRef
                    methodOwner
                    (TypedEvidenceParameterId 0)
                )
            )
            constraint
            importedImplId
            (Just (TypedMethodId importedImplId "render"))
        )
    evidenceSelections
      | includeEvidence = [selectedEvidence]
      | otherwise = []
    importedMethodInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        [ TypedInstantiation
            methodOwner
            [TypedTypeArgument parameter TypedBoolType]
            Nothing
        ]
        evidenceSelections
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["render"])]
        []
        emptyInterface
        [expressionStatement 1 (fixtureVariableExpr importedMethodInfo importedMethod)]
        importedMethodInfo

fractionalLiteralSuffixProgram :: TypedProgram
fractionalLiteralSuffixProgram =
  expressionFixtureProgram
    "review-fractional-literal-suffix"
    ( TypedLiteralExpr
        (info (TypedNumericType TypedFloat64Type) (TypedFloatRecipe 64))
        (TypedFractionalLiteral "1" "5" (Just TypedFloat16Type))
    )

fixturePrelude :: TypedModule
fixturePrelude =
  typedModule
    ["Prelude"]
    (TypedSourcePath "src/Prelude.jz")
    []
    [ TypedModuleExport TypedCapabilityNamespace "Equal",
      TypedModuleExport TypedCapabilityNamespace "Render"
    ]
    ( TypedModuleInterface
        []
        []
        [TypedClassInterface equalityClass, TypedClassInterface renderClass]
        [TypedImplInterface boolImpl, TypedImplInterface charImpl, TypedImplInterface textRenderImpl]
    )
    [ TypedClassStatement equalityClass,
      TypedClassStatement renderClass,
      TypedImplStatement (TypedImplDeclaration span1 boolImpl [equalImplMethod, otherBoolImplMethod]),
      TypedImplStatement (TypedImplDeclaration span1 charImpl [equalCharImplMethod, otherCharImplMethod]),
      TypedImplStatement (TypedImplDeclaration span1 textRenderImpl [renderImplMethod, mapImplMethod])
    ]
    boolInfo
  where
    parameter = TypedTypeParameterId 0
    equalClassName = resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    renderClassName = resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    equalName = resolved TypedCurrentModule TypedValueNamespace "equal"
    otherName = resolved TypedCurrentModule TypedValueNamespace "other"
    renderName = resolved TypedCurrentModule TypedValueNamespace "render"
    mapName = resolved TypedCurrentModule TypedValueNamespace "map"
    renderOwner = binder ["Prelude"] [1, 0] renderName
    mapOwner = binder ["Prelude"] [1, 1] mapName
    equalityClass = fixtureEqualClass TypedCurrentModule
    renderClass =
      TypedClassDeclaration
        span1
        renderClassName
        [parameter]
        [ TypedMethodSignature renderName span1 (fixtureScheme renderOwner [] [] [] boolToBoolType boolToBoolRecipe),
          TypedMethodSignature mapName span1 (fixtureScheme mapOwner [] [] [] genericMapType genericMapRecipe)
        ]
    boolImpl = TypedImplId ["Prelude"] equalClassName [TypedBoolType]
    charImpl = TypedImplId ["Prelude"] equalClassName [TypedCharType]
    textRenderImpl = TypedImplId ["Prelude"] renderClassName [TypedTextType]
    genericMapType =
      TypedFunctionType
        (TypedFunctionType TypedBoolType (TypedTypeParameterType parameter))
        ( TypedFunctionType
            (TypedListType TypedBoolType)
            (TypedListType (TypedTypeParameterType parameter))
        )
    genericMapRecipe =
      TypedClosureRecipe
        [ TypedClosureRecipe
            [TypedBoolRecipe]
            (TypedRepresentationParameterRecipe parameter),
          TypedManagedListRecipe TypedBoolRecipe
        ]
        (TypedManagedListRecipe (TypedRepresentationParameterRecipe parameter))
    equalImplMethod = TypedMethodDefinition (TypedMethodId boolImpl "equal") (binder ["Prelude"] [2, 0] equalName) equalName span1 trueExpr
    otherBoolArgument = resolved TypedCurrentModule TypedValueNamespace "otherBoolArgument"
    otherBoolExpression = TypedLambdaExpr boolToBoolInfo (binder ["Prelude"] [2, 1, 0] otherBoolArgument) otherBoolArgument trueExpr
    otherBoolImplMethod = TypedMethodDefinition (TypedMethodId boolImpl "other") (binder ["Prelude"] [2, 1] otherName) otherName span1 otherBoolExpression
    equalCharImplMethod = TypedMethodDefinition (TypedMethodId charImpl "equal") (binder ["Prelude"] [3, 0] equalName) equalName span1 trueExpr
    otherCharArgument = resolved TypedCurrentModule TypedValueNamespace "otherCharArgument"
    otherCharExpression = TypedLambdaExpr boolToBoolInfo (binder ["Prelude"] [3, 1, 0] otherCharArgument) otherCharArgument trueExpr
    otherCharImplMethod = TypedMethodDefinition (TypedMethodId charImpl "other") (binder ["Prelude"] [3, 1] otherName) otherName span1 otherCharExpression
    renderArgument = resolved TypedCurrentModule TypedValueNamespace "renderArgument"
    renderExpression = TypedLambdaExpr boolToBoolInfo (binder ["Prelude"] [4, 0, 0] renderArgument) renderArgument trueExpr
    renderImplMethod = TypedMethodDefinition (TypedMethodId textRenderImpl "render") (binder ["Prelude"] [4, 0] renderName) renderName span1 renderExpression
    mapExpression = builtinMapDirectExpression ["Prelude"] [4, 1]
    mapImplMethod = TypedMethodDefinition (TypedMethodId textRenderImpl "map") (binder ["Prelude"] [4, 1] mapName) mapName span1 mapExpression

fixtureImplMethod :: [Text] -> [Int] -> TypedImplId -> Text -> TypedMethodDefinition
fixtureImplMethod modulePath methodPath implId methodKey =
  TypedMethodDefinition
    (TypedMethodId implId methodKey)
    (binder modulePath methodPath methodName)
    methodName
    span1
    methodExpression
  where
    methodName = resolved TypedCurrentModule TypedValueNamespace methodKey
    argumentName = resolved TypedCurrentModule TypedValueNamespace (methodKey <> "Argument")
    methodExpression
      | methodKey == "equal" = trueExpr
      | methodKey == "map" = builtinMapDirectExpression modulePath methodPath
      | otherwise =
          TypedLambdaExpr
            boolToBoolInfo
            (binder modulePath (methodPath <> [0]) argumentName)
            argumentName
            trueExpr

withFixturePrelude :: TypedProgram -> TypedProgram
withFixturePrelude (TypedProgram _ modules entryModule) =
  TypedProgram (Just fixturePrelude) modules entryModule

missingPreludeImplId :: TypedImplId
missingPreludeImplId =
  TypedImplId
    ["Prelude"]
    (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal")
    [TypedTextType]

missingPreludeImplProgram :: TypedProgram
missingPreludeImplProgram =
  TypedProgram (Just fixturePrelude) [entryModule] modulePath
  where
    fixture = "review-missing-prelude-impl"
    modulePath = (fixtureModulePath fixture)
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedTextType
    evidence = TypedSelectedEvidence (TypedEvidenceUse Nothing constraint missingPreludeImplId Nothing)
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence]) (TypedBooleanLiteral True)
    entryModule = typedModule modulePath relativeSource [] [] emptyInterface [expressionStatement 1 expression] boolInfo

evidenceTypeScopeParameter :: TypedTypeParameterId
evidenceTypeScopeParameter = TypedTypeParameterId 0

evidenceTypeScopeProgram :: TypedProgram
evidenceTypeScopeProgram =
  TypedProgram (Just fixturePrelude) [entryModule] modulePath
  where
    fixture = "review-evidence-type-scope"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "generic"
    owner = binder modulePath [0] valueName
    parameterType = TypedTypeParameterType evidenceTypeScopeParameter
    scheme = fixtureScheme owner [evidenceTypeScopeParameter] [] [] TypedBoolType TypedBoolRecipe
    implId =
      TypedImplId
        ["Prelude"]
        (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal")
        [TypedBoolType]
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing parameterType
    evidence = TypedSelectedEvidence (TypedEvidenceUse Nothing constraint implId Nothing)
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence]) (TypedBooleanLiteral True)
    entryModule =
      typedModule
        modulePath
        relativeSource
        []
        []
        emptyInterface
        [TypedLetStatement owner valueName span1 scheme expression]
        boolInfo

wrongConstructorDataName :: TypedCoreName
wrongConstructorDataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"

wrongConstructorDataType :: TypedType
wrongConstructorDataType = TypedDataType wrongConstructorDataName []

wrongConstructorPatternTypeProgram :: TypedProgram
wrongConstructorPatternTypeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-wrong-constructor-pattern-type"
    modulePath = (fixtureModulePath fixture)
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "On"
    declaration =
      TypedDataDeclaration
        span1
        wrongConstructorDataName
        []
        [TypedConstructorDeclaration (binder modulePath [0, 0] constructorName) constructorName [] []]
    patternValue = TypedConstructorPattern boolInfo constructorName []
    expression = TypedPatternCaseExpr boolInfo trueExpr [TypedCaseArm patternValue Nothing trueExpr]
    statements = [TypedDataStatement declaration, expressionStatement 1 expression]

foreignOwnedLocalImplId :: TypedImplId
foreignOwnedLocalImplId =
  TypedImplId
    ["Other", "Owner"]
    (resolved TypedCurrentModule TypedCapabilityNamespace "Marker")
    [TypedBoolType]

foreignOwnedLocalImplProgram :: TypedProgram
foreignOwnedLocalImplProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-foreign-owned-local-impl"
    modulePath = (fixtureModulePath fixture)
    className = resolved TypedCurrentModule TypedCapabilityNamespace "Marker"
    statements =
      [ TypedClassStatement (TypedClassDeclaration span1 className [TypedTypeParameterId 0] []),
        TypedImplStatement (TypedImplDeclaration span1 foreignOwnedLocalImplId [])
      ]

importedTypeCapabilityMetadataProgram :: TypedProgram
importedTypeCapabilityMetadataProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "TypeCapability")
    entryPath = (fixtureModulePath "review-imported-type-capability-metadata")
    localClassName = resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    importedClassName = resolved (TypedImportedModule libraryPath) TypedCapabilityNamespace "Render"
    parameter = TypedTypeParameterId 0
    declaration = TypedClassDeclaration span1 localClassName [parameter] []
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/TypeCapability.jz")
        []
        [TypedModuleExport TypedTypeNamespace "Render"]
        (TypedModuleInterface [] [] [TypedClassInterface declaration] [])
        [TypedClassStatement declaration]
        boolInfo
    implId = TypedImplId entryPath importedClassName [TypedBoolType, TypedCharType]
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["Render"])]
        []
        emptyInterface
        [TypedImplStatement (TypedImplDeclaration span1 implId [])]
        boolInfo

callableBuiltinEqualityProgram :: TypedProgram
callableBuiltinEqualityProgram =
  expressionFixtureProgram "review-callable-builtin-equality" expression
  where
    modulePath = (fixtureModulePath "review-callable-builtin-equality")
    argumentName index = resolved TypedCurrentModule TypedValueNamespace ("argument" <> Text.pack (show index))
    function index =
      let name = argumentName index
       in TypedLambdaExpr boolToBoolInfo (binder modulePath [index] name) name trueExpr
    expression = TypedBinaryExpr boolInfo (TypedBuiltinOperator "==") (function 0) (function 1)

moduleInfoStructuralEqualityUnknownOwner :: TypedBinderId
moduleInfoStructuralEqualityUnknownOwner =
  fixtureBinder
    "review-module-info-structural-equality"
    9
    (fixtureValueName "unknown")

moduleInfoStructuralEqualityProgram :: TypedProgram
moduleInfoStructuralEqualityProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 trueExpr] emptyInterface moduleInfo modulePath
  where
    fixture = "review-module-info-structural-equality"
    modulePath = (fixtureModulePath fixture)
    moduleInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [TypedInstantiation moduleInfoStructuralEqualityUnknownOwner [] Nothing]
        []

typeApplicationResultContractProgram :: TypedProgram
typeApplicationResultContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface applicationInfo modulePath
  where
    fixture = "review-type-application-result-contract"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "identity"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    scheme =
      fixtureClosureScheme
        owner
        [parameter]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    instantiation = TypedInstantiation owner [TypedTypeArgument parameter TypedBoolType] (Just span1)
    calleeInfo = TypedNodeInfo boolToBoolType boolToBoolRecipe [instantiation] []
    applicationInfo = TypedNodeInfo TypedTextType TypedManagedTextRecipe [instantiation] []
    expression =
      TypedTypeApplicationExpr
        applicationInfo
        (fixtureVariableExpr calleeInfo valueName)
        span1
        TypedBoolType
    statements =
      [ TypedLetStatement owner valueName span1 scheme (polymorphicIdentityExpression modulePath [0] parameter),
        expressionStatement 1 expression
      ]

capabilityConstraintVisibilityProgram :: TypedProgram
capabilityConstraintVisibilityProgram =
  withFixturePrelude (signatureProgram fixture owner valueName scheme)
  where
    fixture = "review-capability-constraint-visibility"
    valueName = fixtureValueName "constrained"
    owner = fixtureBinder fixture 0 valueName
    evidence =
      [ TypedEvidenceParameter
          (TypedEvidenceParameterId 0)
          (TypedCapabilityConstraint (preludeCapability "Missing") (Just "Missing.m") TypedBoolType),
        TypedEvidenceParameter
          (TypedEvidenceParameterId 1)
          (TypedCapabilityConstraint (preludeCapability "Equal") (Just "Equal.missing") TypedBoolType)
      ]
    scheme = fixtureScheme owner [] evidence [] TypedBoolType TypedBoolRecipe

unconstrainedNumericParameterProgram :: TypedProgram
unconstrainedNumericParameterProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-unconstrained-numeric-parameter"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "numeric"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    parameterInfo = info parameterType parameterRecipe
    argumentName = resolved TypedCurrentModule TypedValueNamespace "operand"
    argumentOwner = binder modulePath [0, 0] argumentName
    argument = fixtureBoundVariableExpr argumentOwner parameterInfo argumentName
    body = TypedBinaryExpr parameterInfo (TypedBuiltinOperator "+") argument argument
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    expression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        argumentOwner
        argumentName
        body
    scheme = fixtureScheme owner [parameter] [] [] functionType functionRecipe
    statement = TypedLetStatement owner valueName span1 scheme expression

unconstrainedEqualityParameterProgram :: TypedProgram
unconstrainedEqualityParameterProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-unconstrained-equality-parameter"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "equal"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterInfo =
      info
        (TypedTypeParameterType parameter)
        (TypedRepresentationParameterRecipe parameter)
    argumentName = resolved TypedCurrentModule TypedValueNamespace "operand"
    argumentOwner = binder modulePath [0, 0] argumentName
    argument = fixtureBoundVariableExpr argumentOwner parameterInfo argumentName
    body = TypedBinaryExpr boolInfo (TypedBuiltinOperator "==") argument argument
    functionType = TypedFunctionType (TypedTypeParameterType parameter) TypedBoolType
    functionRecipe =
      TypedClosureRecipe
        [TypedRepresentationParameterRecipe parameter]
        TypedBoolRecipe
    expression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        argumentOwner
        argumentName
        body
    scheme = fixtureScheme owner [parameter] [] [] functionType functionRecipe
    statement = TypedLetStatement owner valueName span1 scheme expression

duplicatePatternNameSecondBinder :: TypedBinderId
duplicatePatternNameSecondBinder =
  binder
    (fixtureModulePath "review-duplicate-pattern-name")
    [0, 1]
    (fixtureValueName "duplicate")

duplicatePatternNameProgram :: TypedProgram
duplicatePatternNameProgram =
  expressionFixtureProgram fixture expression
  where
    fixture = "review-duplicate-pattern-name"
    modulePath = (fixtureModulePath fixture)
    duplicateName = fixtureValueName "duplicate"
    firstBinder = binder modulePath [0, 0] duplicateName
    patternValue =
      TypedTuplePattern
        pairInfo
        [ TypedVariablePattern boolInfo firstBinder duplicateName,
          TypedVariablePattern boolInfo duplicatePatternNameSecondBinder duplicateName
        ]
    scrutinee = TypedTupleExpr pairInfo [trueExpr, falseExpr]
    expression = TypedPatternCaseExpr boolInfo scrutinee [TypedCaseArm patternValue Nothing trueExpr]

duplicateOrPatternContractProgram :: TypedProgram
duplicateOrPatternContractProgram =
  expressionFixtureProgram fixture expression
  where
    fixture = "review-duplicate-or-pattern-contract"
    modulePath = (fixtureModulePath fixture)
    duplicateName = fixtureValueName "duplicate"
    mixedTupleInfo =
      info
        (TypedTupleType [TypedBoolType, TypedTextType])
        (TypedManagedProductRecipe [TypedBoolRecipe, TypedManagedTextRecipe])
    variable lexicalPath valueInfo =
      TypedVariablePattern
        valueInfo
        (binder modulePath lexicalPath duplicateName)
        duplicateName
    firstAlternative =
      TypedTuplePattern
        mixedTupleInfo
        [variable [0, 0] boolInfo, variable [0, 1] textInfo]
    secondAlternative =
      TypedTuplePattern
        mixedTupleInfo
        [variable [1, 0] boolInfo, variable [1, 1] boolInfo]
    patternValue =
      TypedOrPattern mixedTupleInfo [firstAlternative, secondAlternative]
    scrutinee =
      TypedTupleExpr
        mixedTupleInfo
        [trueExpr, literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "value")]
    expression =
      TypedPatternCaseExpr
        boolInfo
        scrutinee
        [TypedCaseArm patternValue Nothing trueExpr]

nonTuplePatternProgram :: TypedProgram
nonTuplePatternProgram =
  expressionFixtureProgram
    "review-non-tuple-pattern"
    (TypedPatternCaseExpr boolInfo trueExpr [TypedCaseArm (TypedTuplePattern boolInfo []) Nothing trueExpr])

ownerAmbiguousEvidenceProgram :: TypedProgram
ownerAmbiguousEvidenceProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-owner-ambiguous-evidence"
    modulePath = (fixtureModulePath fixture)
    firstName = fixtureValueName "first"
    secondName = fixtureValueName "second"
    firstOwner = fixtureBinder fixture 0 firstName
    secondOwner = ownerAmbiguousSecondOwner
    parameter = TypedTypeParameterId 0
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    scheme owner =
      fixtureScheme
        owner
        [parameter]
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    instantiate owner =
      TypedInstantiation owner [TypedTypeArgument parameter TypedBoolType] Nothing
    implId =
      TypedImplId
        ["Prelude"]
        (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal")
        [TypedBoolType]
    evidenceUse =
      TypedEvidenceUse
        (Just (TypedEvidenceParameterRef firstOwner (TypedEvidenceParameterId 0)))
        constraint
        implId
        Nothing
    expressionInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [instantiate firstOwner, instantiate secondOwner]
        [TypedSelectedEvidence evidenceUse]
    expression = fixtureVariableExpr expressionInfo firstName
    statements =
      [ TypedLetStatement firstOwner firstName span1 (scheme firstOwner) trueExpr,
        TypedLetStatement secondOwner secondName span1 (scheme secondOwner) trueExpr,
        expressionStatement 2 expression
      ]

ownerAmbiguousSecondOwner :: TypedBinderId
ownerAmbiguousSecondOwner =
  fixtureBinder
    "review-owner-ambiguous-evidence"
    1
    (fixtureValueName "second")

reorderedOrPatternProgram :: TypedProgram
reorderedOrPatternProgram =
  expressionFixtureProgram fixture expression
  where
    fixture = "review-reordered-or-pattern"
    modulePath = (fixtureModulePath fixture)
    leftName = fixtureValueName "left"
    rightName = fixtureValueName "right"
    variable lexicalPath name =
      TypedVariablePattern boolInfo (binder modulePath lexicalPath name) name
    firstAlternative =
      TypedTuplePattern
        pairInfo
        [variable [0, 0] leftName, variable [0, 1] rightName]
    secondAlternative =
      TypedTuplePattern
        pairInfo
        [variable [1, 0] rightName, variable [1, 1] leftName]
    patternValue = TypedOrPattern pairInfo [firstAlternative, secondAlternative]
    scrutinee = TypedTupleExpr pairInfo [trueExpr, falseExpr]
    expression = TypedPatternCaseExpr boolInfo scrutinee [TypedCaseArm patternValue Nothing trueExpr]

reorderedOrPatternMismatchBinder :: TypedBinderId
reorderedOrPatternMismatchBinder =
  binder
    (fixtureModulePath "review-reordered-or-pattern")
    [1, 0]
    (fixtureValueName "right")

emptyPatternCaseProgram :: TypedProgram
emptyPatternCaseProgram =
  expressionFixtureProgram
    "review-empty-pattern-case"
    (TypedPatternCaseExpr boolInfo trueExpr [])

typeVisibleImplImportProgram :: TypedProgram
typeVisibleImplImportProgram =
  visibleClassImplImportProgram
    "review-type-visible-impl-import"
    [TypedModuleExport TypedTypeNamespace "Render"]
    ["Render"]

methodVisibleImplImportProgram :: TypedProgram
methodVisibleImplImportProgram =
  visibleClassImplImportProgram
    "review-method-visible-impl-import"
    [TypedModuleExport TypedValueNamespace "render"]
    ["render"]

visibleClassImplImportProgram :: Text -> [TypedModuleExport] -> [Text] -> TypedProgram
visibleClassImplImportProgram fixture exports selectedNames =
  TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath fixture)
    entryPath = (fixtureModulePath fixture)
    localClassName = resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    importedClassName = resolved (TypedImportedModule libraryPath) TypedCapabilityNamespace "Render"
    methodName = resolved TypedCurrentModule TypedValueNamespace "render"
    methodOwner = binder libraryPath [0, 0] methodName
    parameter = TypedTypeParameterId 0
    methodScheme = fixtureScheme methodOwner [] [] [] boolToBoolType boolToBoolRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        localClassName
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    localImplId = TypedImplId libraryPath localClassName [TypedBoolType]
    importedImplId = TypedImplId libraryPath importedClassName [TypedBoolType]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/VisibleClassImpl.jz")
        []
        exports
        ( TypedModuleInterface
            []
            []
            [TypedClassInterface classDeclaration]
            [TypedImplInterface localImplId]
        )
        [ TypedClassStatement classDeclaration,
          TypedImplStatement
            ( TypedImplDeclaration
                span1
                localImplId
                [fixtureImplMethod libraryPath [1, 0] localImplId "render"]
            )
        ]
        boolInfo
    constraint = TypedCapabilityConstraint importedClassName Nothing TypedBoolType
    evidence =
      TypedSelectedEvidence
        (TypedEvidenceUse Nothing constraint importedImplId Nothing)
    expression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence])
        (TypedBooleanLiteral True)
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just selectedNames)]
        []
        emptyInterface
        [expressionStatement 1 expression]
        boolInfo

integralLiteralRangeProgram :: TypedProgram
integralLiteralRangeProgram =
  signatureProgram fixture owner valueName scheme
  where
    fixture = "review-integral-literal-range"
    valueName = fixtureValueName "bounded"
    owner = fixtureBinder fixture 0 valueName
    scheme =
      fixtureScheme
        owner
        []
        []
        [ TypedNumericPrimitiveConstraint
            (TypedIntegralLiteralNumericConstraint "0" "300")
            (TypedNumericType TypedUInt8Type)
        ]
        TypedBoolType
        TypedBoolRecipe

nestedStrictEqualityOperandType :: TypedType
nestedStrictEqualityOperandType =
  TypedListType
    ( TypedTupleType
        [ TypedTypeParameterType (TypedTypeParameterId 0),
          TypedBoolType
        ]
    )

nestedStrictEqualityConstraintProgram :: TypedProgram
nestedStrictEqualityConstraintProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-strict-equality-constraint"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "compare"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    constrainedType = TypedTupleType [parameterType, TypedBoolType]
    operandType = nestedStrictEqualityOperandType
    operandRecipe =
      TypedManagedListRecipe
        (TypedManagedProductRecipe [TypedRepresentationParameterRecipe parameter, TypedBoolRecipe])
    operandInfo = info operandType operandRecipe
    argumentName = resolved TypedCurrentModule TypedValueNamespace "operand"
    argumentBinder = binder modulePath [0, 0] argumentName
    argument = fixtureBoundVariableExpr argumentBinder operandInfo argumentName
    body = TypedBinaryExpr boolInfo (TypedBuiltinOperator "==") argument argument
    lambdaType = TypedFunctionType operandType TypedBoolType
    lambdaRecipe = TypedClosureRecipe [operandRecipe] TypedBoolRecipe
    expression = TypedLambdaExpr (info lambdaType lambdaRecipe) argumentBinder argumentName body
    scheme =
      fixtureScheme
        owner
        [parameter]
        []
        [TypedStrictEqualityPrimitiveConstraint constrainedType]
        lambdaType
        lambdaRecipe
    statement = TypedLetStatement owner valueName span1 scheme expression

qualifiedMethodKeyProgram :: Text -> Text -> TypedProgram
qualifiedMethodKeyProgram fixture methodKey =
  withFixturePrelude (expressionFixtureProgram fixture expression)
  where
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") (Just methodKey) TypedBoolType
    implId = TypedImplId ["Prelude"] capabilityName [TypedBoolType]
    methodId = TypedMethodId implId "equal"
    evidenceUse = TypedEvidenceUse Nothing constraint implId (Just methodId)
    expression
      | methodKey == "Equal::equal" =
          fixtureVariableExpr
            (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
            (TypedBuiltinName "Equal::equal")
      | otherwise =
          TypedLiteralExpr
            (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [TypedSelectedEvidence evidenceUse])
            (TypedBooleanLiteral True)

canonicalQualifiedMethodKeyProgram :: TypedProgram
canonicalQualifiedMethodKeyProgram =
  qualifiedMethodKeyProgram "review-canonical-qualified-method-key" "Equal::equal"

wrongQualifiedMethodKeyProgram :: TypedProgram
wrongQualifiedMethodKeyProgram =
  qualifiedMethodKeyProgram "review-wrong-qualified-method-key" "Other.equal"

builtinValueContractProgram :: TypedProgram
builtinValueContractProgram =
  expressionFixtureProgram
    "review-builtin-value-contract"
    (fixtureVariableExpr boolInfo (TypedBuiltinName "__kernel_textLength"))

missingInterfaceMetadataDataName :: TypedCoreName
missingInterfaceMetadataDataName =
  resolved TypedCurrentModule TypedTypeNamespace "Box"

missingInterfaceMetadataProgram :: TypedProgram
missingInterfaceMetadataProgram =
  TypedProgram Nothing [libraryModule] libraryPath
  where
    libraryPath = (fixtureLibraryPath "MissingMetadata")
    valueName = resolved TypedCurrentModule TypedValueNamespace "boxed"
    valueBinder = binder libraryPath [0] valueName
    dataType = TypedDataType missingInterfaceMetadataDataName []
    dataRecipe = TypedManagedVariantRecipe missingInterfaceMetadataDataName []
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Box"
    valueScheme = fixtureScheme valueBinder [] [] [] dataType dataRecipe
    dataDeclaration =
      dataDeclarationWithNullaryConstructor
        libraryPath
        [1, 0]
        missingInterfaceMetadataDataName
        []
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/MissingMetadata.jz")
        []
        [TypedModuleExport TypedValueNamespace "boxed"]
        (TypedModuleInterface [TypedValueInterface valueName valueScheme] [] [] [])
        [ TypedLetStatement
            valueBinder
            valueName
            span1
            valueScheme
            (fixtureBoundVariableExpr (binder libraryPath [1, 0] constructorName) (info dataType dataRecipe) constructorName),
          TypedDataStatement dataDeclaration
        ]
        boolInfo

unterminatedBlockProgram :: TypedProgram
unterminatedBlockProgram =
  expressionFixtureProgram
    "review-unterminated-block"
    (TypedBlockExpr boolInfo [])

constrainedMonomorphicOwner :: TypedBinderId
constrainedMonomorphicOwner =
  fixtureBinder
    "review-constrained-monomorphic-use"
    0
    (fixtureValueName "same")

constrainedMonomorphicUseProgram :: TypedProgram
constrainedMonomorphicUseProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-constrained-monomorphic-use"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "same"
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    scheme =
      fixtureScheme
        constrainedMonomorphicOwner
        []
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    expression = fixtureBoundVariableExpr constrainedMonomorphicOwner boolInfo valueName
    statements =
      [ TypedLetStatement constrainedMonomorphicOwner valueName span1 scheme trueExpr,
        expressionStatement 1 expression
      ]

unrelatedKnownInstantiationOwner :: TypedBinderId
unrelatedKnownInstantiationOwner =
  fixtureBinder
    "review-unrelated-known-instantiation"
    0
    (fixtureValueName "known")

unrelatedKnownInstantiationProgram :: TypedProgram
unrelatedKnownInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-unrelated-known-instantiation"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "known"
    instantiation = TypedInstantiation unrelatedKnownInstantiationOwner [] Nothing
    expression =
      TypedLiteralExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] [])
        (TypedBooleanLiteral True)
    statements =
      [ TypedLetStatement unrelatedKnownInstantiationOwner valueName span1 (monoScheme unrelatedKnownInstantiationOwner) trueExpr,
        expressionStatement 1 expression
      ]

explicitHeadParameterOwner :: TypedBinderId
explicitHeadParameterOwner =
  fixtureBinder
    "review-explicit-head-parameter"
    0
    (fixtureValueName "choose")

explicitHeadParameterProgram :: TypedProgram
explicitHeadParameterProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface instantiatedInfo modulePath
  where
    fixture = "review-explicit-head-parameter"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "choose"
    firstParameter = TypedTypeParameterId 0
    secondParameter = TypedTypeParameterId 1
    parameterType = TypedTypeParameterType firstParameter
    parameterRecipe = TypedRepresentationParameterRecipe firstParameter
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    scheme =
      fixtureClosureScheme
        explicitHeadParameterOwner
        [firstParameter, secondParameter]
        []
        []
        functionType
        functionRecipe
    instantiation =
      TypedInstantiation
        explicitHeadParameterOwner
        [ TypedTypeArgument firstParameter TypedTextType,
          TypedTypeArgument secondParameter TypedBoolType
        ]
        (Just span1)
    instantiatedInfo =
      TypedNodeInfo
        (TypedFunctionType TypedTextType TypedTextType)
        (TypedClosureRecipe [TypedManagedTextRecipe] TypedManagedTextRecipe)
        [instantiation]
        []
    expression =
      TypedTypeApplicationExpr
        instantiatedInfo
        (fixtureVariableExpr instantiatedInfo valueName)
        span1
        TypedBoolType
    statements =
      [ TypedLetStatement
          explicitHeadParameterOwner
          valueName
          span1
          scheme
          (polymorphicIdentityExpression modulePath [0] firstParameter),
        expressionStatement 1 expression
      ]

classArityProgram :: TypedProgram
classArityProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-class-arity"
    modulePath = (fixtureModulePath fixture)
    zeroName = resolved TypedCurrentModule TypedCapabilityNamespace "Zero"
    multiName = resolved TypedCurrentModule TypedCapabilityNamespace "Multi"
    statements =
      [ TypedClassStatement (TypedClassDeclaration span1 zeroName [] []),
        TypedClassStatement
          ( TypedClassDeclaration
              span1
              multiName
              [TypedTypeParameterId 0, TypedTypeParameterId 1]
              []
          )
      ]

classMethodSchemeShapeProgram :: TypedProgram
classMethodSchemeShapeProgram =
  singleModuleProgram fixture relativeSource [] [TypedClassStatement declaration] emptyInterface boolInfo modulePath
  where
    fixture = "review-class-method-scheme-shape"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Marker"
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    method name scheme =
      TypedMethodSignature
        name
        span1
        scheme
    methodName suffix = resolved TypedCurrentModule TypedValueNamespace suffix
    methodBinder index name = binder modulePath [0, index] name
    localName = methodName "local"
    evidenceName = methodName "evidence"
    primitiveName = methodName "primitive"
    localScheme =
      fixtureScheme
        (methodBinder 0 localName)
        [parameter]
        []
        []
        parameterType
        parameterRecipe
    evidenceScheme =
      fixtureScheme
        (methodBinder 1 evidenceName)
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint capabilityName Nothing parameterType)
        ]
        []
        parameterType
        parameterRecipe
    primitiveScheme =
      fixtureScheme
        (methodBinder 2 primitiveName)
        []
        []
        [TypedStrictEqualityPrimitiveConstraint parameterType]
        parameterType
        parameterRecipe
    declaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [ method localName localScheme,
          method evidenceName evidenceScheme,
          method primitiveName primitiveScheme
        ]

duplicateImplDeclarationId :: TypedImplId
duplicateImplDeclarationId =
  TypedImplId
    (fixtureModulePath "review-duplicate-impl-declaration")
    (resolved TypedCurrentModule TypedCapabilityNamespace "Marker")
    [TypedBoolType]

duplicateImplDeclarationProgram :: TypedProgram
duplicateImplDeclarationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-duplicate-impl-declaration"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Marker"
    declaration =
      TypedClassDeclaration
        span1
        capabilityName
        [TypedTypeParameterId 0]
        []
    implStatement =
      TypedImplStatement
        (TypedImplDeclaration span1 duplicateImplDeclarationId [])
    statements =
      [TypedClassStatement declaration, implStatement, implStatement]

emptyOrPatternProgram :: TypedProgram
emptyOrPatternProgram =
  expressionFixtureProgram
    "review-empty-or-pattern"
    ( TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm (TypedOrPattern boolInfo []) Nothing trueExpr]
    )

nonBindingTypeApplicationProgram :: TypedProgram
nonBindingTypeApplicationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-non-binding-type-application"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "identity"
    owner = fixtureBinder fixture 0 valueName
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    scheme =
      fixtureScheme
        owner
        [parameter]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    instantiation =
      TypedInstantiation
        owner
        [TypedTypeArgument parameter TypedBoolType]
        (Just span1)
    calleeInfo =
      TypedNodeInfo
        boolToBoolType
        boolToBoolRecipe
        [instantiation]
        []
    resultInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [instantiation]
        []
    applied =
      TypedApplyExpr
        resultInfo
        (fixtureVariableExpr calleeInfo valueName)
        trueExpr
    expression =
      TypedTypeApplicationExpr
        resultInfo
        applied
        span1
        TypedBoolType
    statements =
      [ TypedLetStatement owner valueName span1 scheme (polymorphicIdentityExpression modulePath [0] parameter),
        expressionStatement 1 expression
      ]

mismatchedResolvedOperatorProgram :: TypedProgram
mismatchedResolvedOperatorProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface operatorInfo modulePath
  where
    fixture = "review-mismatched-resolved-operator"
    modulePath = (fixtureModulePath fixture)
    operatorName =
      TypedGeneratedName
        (TypedOperatorBinding "$operator:%7E")
    owner = binder modulePath [0] operatorName
    operatorType =
      TypedFunctionType
        TypedBoolType
        (TypedFunctionType TypedBoolType TypedBoolType)
    operatorRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe, TypedBoolRecipe]
        TypedBoolRecipe
    operatorInfo = info operatorType operatorRecipe
    scheme =
      fixtureScheme
        owner
        []
        []
        []
        operatorType
        operatorRecipe
    expression =
      TypedOperatorValueExpr
        operatorInfo
        (TypedResolvedOperator operatorName "^")
    statements =
      [ TypedLetStatement owner operatorName span1 scheme (boolBinaryFunctionExpression modulePath [0]),
        expressionStatement 1 expression
      ]

dataInterfaceDependencyHiddenName :: TypedCoreName
dataInterfaceDependencyHiddenName =
  resolved TypedCurrentModule TypedTypeNamespace "Hidden"

dataInterfaceDependencyProgram :: TypedProgram
dataInterfaceDependencyProgram =
  TypedProgram Nothing [libraryModule] libraryPath
  where
    libraryPath = (fixtureLibraryPath "DataDependency")
    boxName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    boxConstructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Box"
    hiddenType =
      TypedDataType dataInterfaceDependencyHiddenName []
    hiddenDeclaration =
      dataDeclarationWithNullaryConstructor
        libraryPath
        [0, 0]
        dataInterfaceDependencyHiddenName
        []
    boxDeclaration =
      TypedDataDeclaration
        span1
        boxName
        []
        [ TypedConstructorDeclaration
            (binder libraryPath [1, 0] boxConstructorName)
            boxConstructorName
            [hiddenType]
            [TypedManagedVariantRecipe dataInterfaceDependencyHiddenName []]
        ]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/DataDependency.jz")
        []
        [TypedModuleExport TypedTypeNamespace "Box"]
        (TypedModuleInterface [] [TypedDataInterface boxDeclaration] [] [])
        [ TypedDataStatement hiddenDeclaration,
          TypedDataStatement boxDeclaration
        ]
        boolInfo

classMethodInterfaceDependencyDataName :: TypedCoreName
classMethodInterfaceDependencyDataName =
  resolved TypedCurrentModule TypedTypeNamespace "Box"

classMethodInterfaceDependencyProgram :: TypedProgram
classMethodInterfaceDependencyProgram =
  TypedProgram Nothing [libraryModule] libraryPath
  where
    libraryPath = (fixtureLibraryPath "ClassMethodDependency")
    boxDeclaration =
      dataDeclarationWithNullaryConstructor
        libraryPath
        [0, 0]
        classMethodInterfaceDependencyDataName
        []
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    methodName =
      resolved TypedCurrentModule TypedValueNamespace "render"
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    boxType =
      TypedDataType classMethodInterfaceDependencyDataName []
    boxRecipe =
      TypedManagedVariantRecipe
        classMethodInterfaceDependencyDataName
        []
    methodScheme =
      fixtureScheme
        (binder libraryPath [1, 0] methodName)
        []
        []
        []
        (TypedFunctionType boxType parameterType)
        ( TypedClosureRecipe
            [boxRecipe]
            (TypedRepresentationParameterRecipe parameter)
        )
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [TypedMethodSignature methodName span1 methodScheme]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/ClassMethodDependency.jz")
        []
        [TypedModuleExport TypedValueNamespace "render"]
        (TypedModuleInterface [] [] [TypedClassInterface classDeclaration] [])
        [ TypedDataStatement boxDeclaration,
          TypedClassStatement classDeclaration
        ]
        boolInfo

instantiatedPrimitiveConstraintProgram :: TypedProgram
instantiatedPrimitiveConstraintProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-instantiated-primitive-constraints"
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    numericName = fixtureValueName "numeric"
    numericOwner = binder modulePath [0] numericName
    equalityName = fixtureValueName "equality"
    equalityOwner = binder modulePath [1] equalityName
    constrainedScheme owner primitiveConstraint =
      fixtureScheme
        owner
        [parameter]
        []
        [primitiveConstraint]
        TypedBoolType
        TypedBoolRecipe
    instantiatedUse owner name typeArgument =
      fixtureVariableExpr
        ( TypedNodeInfo
            TypedBoolType
            TypedBoolRecipe
            [TypedInstantiation owner [TypedTypeArgument parameter typeArgument] Nothing]
            []
        )
        name
    statements =
      [ TypedLetStatement
          numericOwner
          numericName
          span1
          (constrainedScheme numericOwner (TypedNumericPrimitiveConstraint TypedIntegralNumericConstraint parameterType))
          trueExpr,
        TypedLetStatement
          equalityOwner
          equalityName
          span1
          (constrainedScheme equalityOwner (TypedStrictEqualityPrimitiveConstraint parameterType))
          trueExpr,
        expressionStatement 2 (instantiatedUse numericOwner numericName TypedBoolType),
        expressionStatement 3 (instantiatedUse equalityOwner equalityName boolToBoolType)
      ]

typeApplicationExtraOwner :: TypedBinderId
typeApplicationExtraOwner =
  fixtureBinder
    "review-type-application-extra-owner"
    1
    (fixtureValueName "other")

typeApplicationExtraOwnerProgram :: TypedProgram
typeApplicationExtraOwnerProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-type-application-extra-owner"
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    functionName = fixtureValueName "function"
    functionOwner = binder modulePath [0] functionName
    otherName = fixtureValueName "other"
    scheme owner =
      fixtureScheme owner [parameter] [] [] TypedBoolType TypedBoolRecipe
    instantiate owner maybeSpan =
      TypedInstantiation owner [TypedTypeArgument parameter TypedBoolType] maybeSpan
    functionInstantiation = instantiate functionOwner (Just span1)
    otherInstantiation = instantiate typeApplicationExtraOwner Nothing
    functionInfo =
      TypedNodeInfo TypedBoolType TypedBoolRecipe [functionInstantiation] []
    applicationInfo =
      TypedNodeInfo
        TypedBoolType
        TypedBoolRecipe
        [functionInstantiation, otherInstantiation]
        []
    expression =
      TypedTypeApplicationExpr
        applicationInfo
        (fixtureVariableExpr functionInfo functionName)
        span1
        TypedBoolType
    statements =
      [ TypedLetStatement functionOwner functionName span1 (scheme functionOwner) trueExpr,
        TypedLetStatement typeApplicationExtraOwner otherName span1 (scheme typeApplicationExtraOwner) trueExpr,
        expressionStatement 2 expression
      ]

constrainedResolvedOperatorOwner :: TypedBinderId
constrainedResolvedOperatorOwner =
  binder
    (fixtureModulePath "review-constrained-resolved-operator")
    [0]
    constrainedResolvedOperatorName

constrainedResolvedOperatorName :: TypedCoreName
constrainedResolvedOperatorName =
  TypedGeneratedName (TypedOperatorBinding "$operator:%7E")

constrainedResolvedOperatorProgram :: TypedProgram
constrainedResolvedOperatorProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] statements emptyInterface operatorInfo modulePath)
  where
    fixture = "review-constrained-resolved-operator"
    modulePath = (fixtureModulePath fixture)
    operatorType =
      TypedFunctionType
        TypedBoolType
        (TypedFunctionType TypedBoolType TypedBoolType)
    operatorRecipe =
      TypedClosureRecipe
        [TypedBoolRecipe, TypedBoolRecipe]
        TypedBoolRecipe
    operatorInfo = info operatorType operatorRecipe
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    scheme =
      fixtureScheme
        constrainedResolvedOperatorOwner
        []
        [TypedEvidenceParameter (TypedEvidenceParameterId 0) constraint]
        []
        operatorType
        operatorRecipe
    expression =
      TypedOperatorValueExpr
        operatorInfo
        (TypedResolvedOperator constrainedResolvedOperatorName "~")
    statements =
      [ TypedLetStatement
          constrainedResolvedOperatorOwner
          constrainedResolvedOperatorName
          span1
          scheme
          (boolBinaryFunctionExpression modulePath [0]),
        expressionStatement 2 expression
      ]

missingModuleResultProgram :: TypedProgram
missingModuleResultProgram =
  TypedProgram
    Nothing
    [ TypedModule
        (fixtureModulePath fixture)
        relativeSource
        []
        []
        emptyInterface
        []
        [TypedLetStatement owner name span1 (monoScheme owner) trueExpr]
        boolInfo
    ]
    (fixtureModulePath fixture)
  where
    fixture = "review-missing-module-result"
    name = fixtureValueName "item"
    owner = fixtureBinder fixture 0 name

emptyDataDeclarationProgram :: TypedProgram
emptyDataDeclarationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-empty-data-declaration"
    modulePath = (fixtureModulePath fixture)
    dataName = resolved TypedCurrentModule TypedTypeNamespace "Never"
    statements =
      [ TypedDataStatement (TypedDataDeclaration span1 dataName [] []),
        expressionStatement 2 trueExpr
      ]

laterOrPatternCollidingBinder :: TypedBinderId
laterOrPatternCollidingBinder =
  fixtureBinder
    "review-later-or-pattern-binder-collision"
    0
    (fixtureValueName "matched")

laterOrPatternBinderCollisionProgram :: TypedProgram
laterOrPatternBinderCollisionProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-later-or-pattern-binder-collision"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "matched"
    firstBinder = binder modulePath [1, 0] valueName
    firstAlternative = TypedVariablePattern boolInfo firstBinder valueName
    secondAlternative =
      TypedVariablePattern boolInfo laterOrPatternCollidingBinder valueName
    patternValue =
      TypedOrPattern boolInfo [firstAlternative, secondAlternative]
    expression =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm patternValue Nothing trueExpr]
    statements =
      [ TypedLetStatement
          laterOrPatternCollidingBinder
          valueName
          span1
          (monoScheme laterOrPatternCollidingBinder)
          trueExpr,
        expressionStatement 2 expression
      ]

concreteIntegerBoundsProgram :: TypedProgram
concreteIntegerBoundsProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    [ expressionStatement 1 (integerExpression "300"),
      expressionStatement 2 (integerExpression "-1")
    ]
    emptyInterface
    integerInfo
    (fixtureModulePath fixture)
  where
    fixture = "review-concrete-integer-bounds"
    integerInfo =
      info
        (TypedNumericType TypedUInt8Type)
        (TypedUnsignedIntegerRecipe 8)
    integerExpression value =
      TypedLiteralExpr integerInfo (TypedIntegerLiteral value)

incompleteImplProgram :: TypedProgram
incompleteImplProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-incomplete-impl"
    modulePath = (fixtureModulePath fixture)
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Render"
    renderName =
      resolved TypedCurrentModule TypedValueNamespace "render"
    mapName =
      resolved TypedCurrentModule TypedValueNamespace "map"
    parameter = TypedTypeParameterId 0
    methodScheme methodOwner =
      fixtureScheme
        methodOwner
        []
        []
        []
        (TypedFunctionType (TypedTypeParameterType parameter) TypedTextType)
        ( TypedClosureRecipe
            [TypedRepresentationParameterRecipe parameter]
            TypedManagedTextRecipe
        )
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameter]
        [ TypedMethodSignature renderName span1 (methodScheme (binder modulePath [0, 0] renderName)),
          TypedMethodSignature mapName span1 (methodScheme (binder modulePath [0, 1] mapName))
        ]
    implId =
      TypedImplId modulePath capabilityName [TypedBoolType]
    statements =
      [ TypedClassStatement classDeclaration,
        TypedImplStatement (TypedImplDeclaration span1 implId []),
        expressionStatement 3 trueExpr
      ]

duplicateInstantiationOwner :: TypedBinderId
duplicateInstantiationOwner =
  fixtureBinder
    "review-duplicate-instantiation"
    0
    (fixtureValueName "item")

duplicateInstantiationProgram :: TypedProgram
duplicateInstantiationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-duplicate-instantiation"
    modulePath = (fixtureModulePath fixture)
    valueName = fixtureValueName "item"
    parameter = TypedTypeParameterId 0
    scheme =
      fixtureScheme
        duplicateInstantiationOwner
        [parameter]
        []
        []
        TypedBoolType
        TypedBoolRecipe
    instantiate typeValue =
      TypedInstantiation
        duplicateInstantiationOwner
        [TypedTypeArgument parameter typeValue]
        Nothing
    expression =
      fixtureVariableExpr
        ( TypedNodeInfo
            TypedBoolType
            TypedBoolRecipe
            [instantiate TypedBoolType, instantiate TypedTextType]
            []
        )
        valueName
    statements =
      [ TypedLetStatement
          duplicateInstantiationOwner
          valueName
          span1
          scheme
          trueExpr,
        expressionStatement 2 expression
      ]

fractionalLiteralBoundsProgram :: TypedProgram
fractionalLiteralBoundsProgram =
  singleModuleProgram
    fixture
    relativeSource
    []
    [ expressionStatement 1 (fractionalExpression TypedFloat16Type 16 "65504" "0"),
      expressionStatement 2 (fractionalExpression TypedFloat16Type 16 "65504" "1"),
      expressionStatement 3 (fractionalExpression TypedFloat16Type 16 "-65504" "1"),
      expressionStatement 4 (fractionalExpression TypedFloat32Type 32 float32Maximum "0"),
      expressionStatement 5 (fractionalExpression TypedFloat32Type 32 float32Maximum "1"),
      expressionStatement 6 (fractionalExpression TypedFloat64Type 64 float64Maximum "0"),
      expressionStatement 7 (fractionalExpression TypedFloat64Type 64 float64Maximum "1")
    ]
    emptyInterface
    (floatInfo TypedFloat64Type 64)
    (fixtureModulePath fixture)
  where
    fixture = "review-fractional-literal-bounds"
    float32Maximum =
      "340282346638528859811704183484516925440"
    float64Maximum =
      "179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368"
    floatInfo numericType width =
      info
        (TypedNumericType numericType)
        (TypedFloatRecipe width)
    fractionalExpression numericType width whole fractional =
      TypedLiteralExpr
        (floatInfo numericType width)
        (TypedFractionalLiteral whole fractional (Just numericType))

visibleClassCollisionPreludeName :: TypedCoreName
visibleClassCollisionPreludeName =
  resolved TypedCurrentModule TypedCapabilityNamespace "Render"

visibleClassCollisionImportedName :: TypedCoreName
visibleClassCollisionImportedName =
  resolved TypedCurrentModule TypedCapabilityNamespace "Visible"

visibleClassCollisionProgram :: TypedProgram
visibleClassCollisionProgram =
  TypedProgram (Just fixturePrelude) [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "VisibleClass")
    entryPath = (fixtureModulePath "review-visible-class-collision")
    parameter = TypedTypeParameterId 0
    libraryClassName =
      resolved TypedCurrentModule TypedCapabilityNamespace "Visible"
    libraryDeclaration =
      TypedClassDeclaration span1 libraryClassName [parameter] []
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/VisibleClass.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "Visible"]
        (TypedModuleInterface [] [] [TypedClassInterface libraryDeclaration] [])
        [TypedClassStatement libraryDeclaration]
        unitInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["Visible"])]
        []
        emptyInterface
        [ TypedClassStatement
            ( TypedClassDeclaration
                span1
                visibleClassCollisionPreludeName
                [parameter]
                []
            ),
          TypedClassStatement
            ( TypedClassDeclaration
                span1
                visibleClassCollisionImportedName
                [parameter]
                []
            ),
          expressionStatement 3 trueExpr
        ]
        boolInfo

selectedClassDataDependencyCapabilityName :: TypedCoreName
selectedClassDataDependencyCapabilityName =
  resolved
    (TypedImportedModule (fixtureLibraryPath "SelectedClassData"))
    TypedCapabilityNamespace
    "RoundTrip"

selectedClassDataDependencyProgram :: TypedProgram
selectedClassDataDependencyProgram =
  TypedProgram Nothing [libraryModule, facadeModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "SelectedClassData")
    facadePath = (fixtureLibraryPath "SelectedClassDataFacade")
    entryPath = (fixtureModulePath "review-selected-class-data-dependency")
    dataName =
      resolved TypedCurrentModule TypedTypeNamespace "Box"
    constructorName =
      resolved TypedCurrentModule TypedConstructorNamespace "Box"
    dataDeclaration =
      TypedDataDeclaration
        span1
        dataName
        []
        [ TypedConstructorDeclaration
            (binder libraryPath [0, 0] constructorName)
            constructorName
            []
            []
        ]
    capabilityName =
      resolved TypedCurrentModule TypedCapabilityNamespace "RoundTrip"
    methodName =
      resolved TypedCurrentModule TypedValueNamespace "roundTrip"
    classParameter = TypedTypeParameterId 0
    localBoxType = TypedDataType dataName []
    localBoxRecipe = TypedManagedVariantRecipe dataName []
    methodOwner = binder libraryPath [1, 0] methodName
    methodScheme =
      fixtureScheme
        methodOwner
        []
        []
        []
        (TypedFunctionType localBoxType localBoxType)
        (TypedClosureRecipe [localBoxRecipe] localBoxRecipe)
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [classParameter]
        [TypedMethodSignature methodName span1 methodScheme]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/SelectedClassData.jz")
        []
        [TypedModuleExport TypedCapabilityNamespace "RoundTrip"]
        ( TypedModuleInterface
            []
            [TypedDataInterface dataDeclaration]
            [TypedClassInterface classDeclaration]
            []
        )
        [ TypedDataStatement dataDeclaration,
          TypedClassStatement classDeclaration
        ]
        unitInfo
    importedCapabilityName = selectedClassDataDependencyCapabilityName
    importedMethodName =
      resolved
        (TypedImportedModule libraryPath)
        TypedValueNamespace
        "roundTrip"
    retainedClass =
      TypedClassDeclaration
        span1
        importedCapabilityName
        [classParameter]
        [TypedMethodSignature importedMethodName span1 methodScheme]
    forwardedName =
      resolved TypedCurrentModule TypedValueNamespace "forwarded"
    forwardedOwner = binder facadePath [0] forwardedName
    forwardedScheme =
      fixtureScheme
        forwardedOwner
        []
        [ TypedEvidenceParameter
            (TypedEvidenceParameterId 0)
            (TypedCapabilityConstraint importedCapabilityName Nothing TypedBoolType)
        ]
        []
        TypedBoolType
        TypedBoolRecipe
    facadeModule =
      typedModule
        facadePath
        (TypedSourcePath "src/Library/SelectedClassDataFacade.jz")
        [TypedResolvedImport span1 libraryPath Nothing (Just ["RoundTrip"])]
        [TypedModuleExport TypedValueNamespace "forwarded"]
        ( TypedModuleInterface
            [TypedValueInterface forwardedName forwardedScheme]
            []
            [TypedClassInterface retainedClass]
            []
        )
        [TypedLetStatement forwardedOwner forwardedName span1 forwardedScheme trueExpr]
        unitInfo
    importedDataName =
      resolved
        (TypedImportedModule libraryPath)
        TypedTypeNamespace
        "Box"
    importedBoxType = TypedDataType importedDataName []
    importedBoxRecipe = TypedManagedVariantRecipe importedDataName []
    methodType = TypedFunctionType importedBoxType importedBoxType
    methodRecipe = TypedClosureRecipe [importedBoxRecipe] importedBoxRecipe
    methodInfo = info methodType methodRecipe
    implId = TypedImplId entryPath importedCapabilityName [TypedBoolType]
    localMethodName =
      resolved TypedCurrentModule TypedValueNamespace "roundTrip"
    methodBinder = binder entryPath [0, 0] localMethodName
    parameterName =
      resolved TypedCurrentModule TypedValueNamespace "item"
    parameterBinder = binder entryPath [0, 0, 0] parameterName
    body = fixtureBoundVariableExpr parameterBinder (info importedBoxType importedBoxRecipe) parameterName
    methodBody =
      TypedLambdaExpr
        methodInfo
        parameterBinder
        parameterName
        body
    methodDefinition =
      TypedMethodDefinition
        (TypedMethodId implId "roundTrip")
        methodBinder
        localMethodName
        span1
        methodBody
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 facadePath Nothing (Just ["forwarded"])]
        []
        emptyInterface
        [ TypedImplStatement
            (TypedImplDeclaration span1 implId [methodDefinition])
        ]
        unitInfo

selectedValueDataMetadataProgram :: TypedProgram
selectedValueDataMetadataProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "SelectedValueData")
    entryPath = (fixtureModulePath "review-selected-value-data-metadata")
    localDataName = resolved TypedCurrentModule TypedTypeNamespace "Box"
    importedDataName = resolved (TypedImportedModule libraryPath) TypedTypeNamespace "Box"
    localValueName = resolved TypedCurrentModule TypedValueNamespace "boxed"
    importedValueName = resolved (TypedImportedModule libraryPath) TypedValueNamespace "boxed"
    localConstructorName = resolved TypedCurrentModule TypedConstructorNamespace "Box"
    valueBinder = binder libraryPath [0] localValueName
    dataType = TypedDataType localDataName []
    dataRecipe = TypedManagedVariantRecipe localDataName []
    importedType = TypedDataType importedDataName []
    importedRecipe = TypedManagedVariantRecipe importedDataName []
    valueScheme = fixtureScheme valueBinder [] [] [] dataType dataRecipe
    dataDeclaration =
      dataDeclarationWithNullaryConstructor
        libraryPath
        [1, 0]
        localDataName
        []
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/SelectedValueData.jz")
        []
        [TypedModuleExport TypedValueNamespace "boxed"]
        (TypedModuleInterface [TypedValueInterface localValueName valueScheme] [TypedDataInterface dataDeclaration] [] [])
        [ TypedLetStatement
            valueBinder
            localValueName
            span1
            valueScheme
            (fixtureBoundVariableExpr (binder libraryPath [1, 0] localConstructorName) (info dataType dataRecipe) localConstructorName),
          TypedDataStatement dataDeclaration
        ]
        boolInfo
    entryInfo = info importedType importedRecipe
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["boxed"])]
        []
        emptyInterface
        [expressionStatement 1 (fixtureBoundVariableExpr valueBinder entryInfo importedValueName)]
        entryInfo

selectiveImportLeakedImpl :: TypedImplId
selectiveImportLeakedImpl =
  TypedImplId
    (fixtureLibraryPath "PrivateImpl")
    (resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal")
    [TypedBoolType]

selectiveImportImplLeakProgram :: TypedProgram
selectiveImportImplLeakProgram = TypedProgram (Just fixturePrelude) [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "PrivateImpl")
    entryPath = (fixtureModulePath "review-selective-import-impl-leak")
    localValueName = resolved TypedCurrentModule TypedValueNamespace "published"
    valueBinder = binder libraryPath [0] localValueName
    valueScheme = monoScheme valueBinder
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/PrivateImpl.jz")
        []
        [TypedModuleExport TypedValueNamespace "published"]
        ( TypedModuleInterface
            [TypedValueInterface localValueName valueScheme]
            []
            [TypedClassInterface retainedPreludeEqualClass]
            [TypedImplInterface selectiveImportLeakedImpl]
        )
        [ TypedLetStatement valueBinder localValueName span1 valueScheme trueExpr,
          TypedImplStatement
            ( TypedImplDeclaration
                span1
                selectiveImportLeakedImpl
                [ fixtureImplMethod libraryPath [1, 0] selectiveImportLeakedImpl "equal",
                  fixtureImplMethod libraryPath [1, 1] selectiveImportLeakedImpl "other"
                ]
            )
        ]
        boolInfo
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    evidence = TypedSelectedEvidence (TypedEvidenceUse Nothing constraint selectiveImportLeakedImpl Nothing)
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence]) (TypedBooleanLiteral True)
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["published"])]
        []
        emptyInterface
        [expressionStatement 1 expression]
        boolInfo

selectedEvidenceMethodExistenceProgram :: TypedProgram
selectedEvidenceMethodExistenceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-selected-evidence-method-existence"
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    capability = TypedClassDeclaration span1 capabilityName [parameter] []
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    constraint = TypedCapabilityConstraint capabilityName (Just "Equal.equal") TypedBoolType
    methodId = TypedMethodId implId "equal"
    evidence = TypedSelectedEvidence (TypedEvidenceUse Nothing constraint implId (Just methodId))
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence]) (TypedBooleanLiteral True)
    statements =
      [ TypedClassStatement capability,
        TypedImplStatement (TypedImplDeclaration span1 implId []),
        expressionStatement 1 expression
      ]

duplicateImplMethodName :: TypedCoreName
duplicateImplMethodName = resolved TypedCurrentModule TypedValueNamespace "equal"

duplicateImplMethodProgram :: TypedProgram
duplicateImplMethodProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] [TypedImplStatement declaration] emptyInterface boolInfo modulePath)
  where
    fixture = "review-duplicate-impl-method"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    methodId = TypedMethodId implId "equal"
    method lexicalIndex =
      TypedMethodDefinition
        methodId
        (binder modulePath [lexicalIndex] duplicateImplMethodName)
        duplicateImplMethodName
        span1
        trueExpr
    declaration =
      TypedImplDeclaration
        span1
        implId
        [ method 0,
          method 1,
          fixtureImplMethod modulePath [2] implId "other"
        ]

nestedOuterTypeScopeProgram :: TypedProgram
nestedOuterTypeScopeProgram =
  singleModuleProgram fixture relativeSource [] [topLevelBinding] emptyInterface boolInfo modulePath
  where
    fixture = "review-nested-outer-type-scope"
    modulePath = (fixtureModulePath fixture)
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    parameterRecipe = TypedRepresentationParameterRecipe parameter
    parameterInfo = info parameterType parameterRecipe
    outerName = resolved TypedCurrentModule TypedValueNamespace "outer"
    outerBinder = binder modulePath [0] outerName
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentBinder = binder modulePath [0, 0] argumentName
    argumentUse = fixtureBoundVariableExpr argumentBinder parameterInfo argumentName
    localName = resolved TypedCurrentModule TypedValueNamespace "local"
    localBinder = binder modulePath [0, 0, 0] localName
    localScheme = fixtureScheme localBinder [] [] [] parameterType parameterRecipe
    localBinding =
      TypedLetStatement
        localBinder
        localName
        span1
        localScheme
        argumentUse
    localUse = expressionStatement 2 (fixtureBoundVariableExpr localBinder parameterInfo localName)
    block = TypedBlockExpr parameterInfo [localBinding, localUse]
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    expression =
      TypedLambdaExpr
        (info functionType functionRecipe)
        argumentBinder
        argumentName
        block
    outerScheme = fixtureScheme outerBinder [parameter] [] [] functionType functionRecipe
    topLevelBinding = TypedLetStatement outerBinder outerName span1 outerScheme expression

implMethodVisibleName :: TypedCoreName
implMethodVisibleName = resolved TypedCurrentModule TypedValueNamespace "equal"

implMethodValueVisibilityProgram :: TypedProgram
implMethodValueVisibilityProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-impl-method-value-visibility"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    method =
      TypedMethodDefinition
        (TypedMethodId implId "equal")
        (binder modulePath [0] implMethodVisibleName)
        implMethodVisibleName
        span1
        trueExpr
    statements =
      [ TypedImplStatement
          ( TypedImplDeclaration
              span1
              implId
              [method, fixtureImplMethod modulePath [0, 1] implId "other"]
          ),
        expressionStatement 2 (fixtureVariableExpr boolInfo implMethodVisibleName)
      ]

builtinOperatorContractProgram :: TypedProgram
builtinOperatorContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface intInfo modulePath
  where
    fixture = "review-builtin-operator-contract"
    modulePath = (fixtureModulePath fixture)
    intInfo = info TypedIntType (TypedSignedIntegerRecipe 64)
    one = TypedLiteralExpr intInfo (TypedIntegerLiteral "1")
    invalidUnknown = TypedOperatorValueExpr boolToBoolInfo (TypedBuiltinOperator "%%")
    invalidResult = TypedBinaryExpr boolInfo (TypedBuiltinOperator "+") one one
    validResult = TypedBinaryExpr intInfo (TypedBuiltinOperator "+") one one
    statements = [expressionStatement 1 invalidUnknown, expressionStatement 2 invalidResult, expressionStatement 3 validResult]

ordinaryFunctionCandidateAmbiguityProgram :: TypedProgram
ordinaryFunctionCandidateAmbiguityProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-ordinary-function-candidate-ambiguity"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    implId = TypedImplId ["Prelude"] capabilityName [TypedTextType]
    constraint = TypedCapabilityConstraint (preludeCapability "Render") (Just "Render.render") TypedTextType
    candidate = TypedEvidenceCandidate implId (Just (TypedMethodId implId "render"))
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    expression =
      TypedLambdaExpr
        (TypedNodeInfo boolToBoolType boolToBoolRecipe [] [TypedEvidenceCandidates constraint [candidate, candidate]])
        (binder modulePath [0] argumentName)
        argumentName
        trueExpr

invalidNumericPrimitiveConstraintProgram :: TypedProgram
invalidNumericPrimitiveConstraintProgram =
  signatureProgram fixture valueBinder valueName scheme
  where
    fixture = "review-invalid-numeric-primitive-constraint"
    valueName = fixtureValueName "item"
    valueBinder = fixtureBinder fixture 0 valueName
    scheme =
      fixtureScheme
        valueBinder
        []
        []
        [TypedNumericPrimitiveConstraint TypedAnyNumericConstraint TypedTextType]
        TypedBoolType
        TypedBoolRecipe

missingInstantiationDataName :: TypedCoreName
missingInstantiationDataName = resolved TypedCurrentModule TypedTypeNamespace "Missing"

instantiationDataTypeProgram :: TypedProgram
instantiationDataTypeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-instantiation-data-type"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "phantom"
    owner = binder modulePath [0] valueName
    parameterId = TypedTypeParameterId 0
    scheme = fixtureScheme owner [parameterId] [] [] TypedBoolType TypedBoolRecipe
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId (TypedDataType missingInstantiationDataName [])] Nothing
    expression = fixtureVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) valueName
    statements = [TypedLetStatement owner valueName span1 scheme trueExpr, expressionStatement 2 expression]

literalPatternProgram :: TypedProgram
literalPatternProgram =
  expressionFixtureProgram
    "review-literal-pattern"
    ( TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm (TypedLiteralPattern boolInfo (TypedTextLiteral "wrong")) Nothing falseExpr]
    )

invisibleOperatorName :: TypedCoreName
invisibleOperatorName = TypedGeneratedName (TypedOperatorBinding "$operator:%7E")

invisibleOperatorProgram :: TypedProgram
invisibleOperatorProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface boolInfo entryPath
  where
    fixture = "review-invisible-operator"
    entryPath = (fixtureModulePath "review-invisible-operator")
    expression = TypedOperatorValueExpr boolInfo (TypedResolvedOperator invisibleOperatorName "~")

expressionDuplicateBinder :: TypedBinderId
expressionDuplicateBinder =
  binder
    (fixtureModulePath "review-expression-duplicate-binder")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "item")

expressionDuplicateBinderProgram :: TypedProgram
expressionDuplicateBinderProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolToBoolInfo modulePath
  where
    fixture = "review-expression-duplicate-binder"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    scheme = monoScheme expressionDuplicateBinder
    lambda = TypedLambdaExpr boolToBoolInfo expressionDuplicateBinder valueName (fixtureBoundVariableExpr expressionDuplicateBinder boolInfo valueName)
    statements = [TypedLetStatement expressionDuplicateBinder valueName span1 scheme trueExpr, expressionStatement 2 lambda]

privateInterfaceLibraryPath :: [Text]
privateInterfaceLibraryPath = ["Private", "Library"]

privateInterfaceEntryPath :: [Text]
privateInterfaceEntryPath = (fixtureModulePath "review-private-interface")

privateInterfaceLocalName :: TypedCoreName
privateInterfaceLocalName = resolved TypedCurrentModule TypedValueNamespace "secret"

privateInterfaceImportedName :: TypedCoreName
privateInterfaceImportedName = resolved (TypedImportedModule privateInterfaceLibraryPath) TypedValueNamespace "secret"

privateInterfaceLeakProgram :: TypedProgram
privateInterfaceLeakProgram = TypedProgram Nothing [libraryModule, entryModule] privateInterfaceEntryPath
  where
    owner = binder privateInterfaceLibraryPath [0] privateInterfaceLocalName
    scheme = monoScheme owner
    libraryModule =
      typedModule
        privateInterfaceLibraryPath
        (TypedSourcePath "src/Private/Library.jz")
        []
        []
        (TypedModuleInterface [TypedValueInterface privateInterfaceLocalName scheme] [] [] [])
        [TypedLetStatement owner privateInterfaceLocalName span1 scheme trueExpr]
        boolInfo
    entryModule =
      typedModule
        privateInterfaceEntryPath
        relativeSource
        [TypedResolvedImport span1 privateInterfaceLibraryPath Nothing Nothing]
        []
        emptyInterface
        [expressionStatement 1 (fixtureVariableExpr boolInfo privateInterfaceImportedName)]
        boolInfo

constructorPatternContractProgram :: TypedProgram
constructorPatternContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-constructor-pattern-contract"
    modulePath = (fixtureModulePath fixture)
    optionName = resolved TypedCurrentModule TypedTypeNamespace "Option"
    someName = resolved TypedCurrentModule TypedConstructorNamespace "Some"
    parameterId = TypedTypeParameterId 0
    constructorOwner = binder modulePath [0, 0] someName
    declaration =
      TypedDataDeclaration
        span1
        optionName
        [parameterId]
        [ TypedConstructorDeclaration
            constructorOwner
            someName
            [TypedTypeParameterType parameterId]
            [TypedRepresentationParameterRecipe parameterId]
        ]
    optionInfo = info (TypedDataType optionName [TypedBoolType]) (TypedManagedVariantRecipe optionName [TypedBoolType])
    constructorInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (TypedDataType optionName [TypedBoolType]))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedManagedVariantRecipe optionName [TypedBoolType]))
        [TypedInstantiation constructorOwner [TypedTypeArgument parameterId TypedBoolType] Nothing]
        []
    scrutinee = TypedApplyExpr optionInfo (fixtureVariableExpr constructorInfo someName) trueExpr
    expression =
      TypedPatternCaseExpr
        boolInfo
        scrutinee
        [ TypedCaseArm (TypedConstructorPattern optionInfo someName []) Nothing trueExpr,
          TypedCaseArm (TypedConstructorPattern optionInfo someName [TypedWildcardPattern textInfo]) Nothing falseExpr
        ]
    statements = [TypedDataStatement declaration, expressionStatement 2 expression]

nonListPatternProgram :: TypedProgram
nonListPatternProgram =
  expressionFixtureProgram
    "review-non-list-pattern"
    (TypedPatternCaseExpr boolInfo trueExpr [TypedCaseArm (TypedListPattern boolInfo []) Nothing falseExpr])

explicitTypeApplicationOwner :: TypedBinderId
explicitTypeApplicationOwner =
  binder
    (fixtureModulePath "review-explicit-type-application-contract")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "item")

explicitTypeApplicationContractProgram :: TypedProgram
explicitTypeApplicationContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-explicit-type-application-contract"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    scheme = monoScheme explicitTypeApplicationOwner
    expression = TypedTypeApplicationExpr boolInfo (fixtureBoundVariableExpr explicitTypeApplicationOwner boolInfo valueName) span1 TypedBoolType
    statements = [TypedLetStatement explicitTypeApplicationOwner valueName span1 scheme trueExpr, expressionStatement 2 expression]

variableSchemeContractProgram :: TypedProgram
variableSchemeContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textInfo modulePath
  where
    fixture = "review-variable-scheme-contract"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "item"
    valueBinder = binder modulePath [0] valueName
    statements =
      [ TypedLetStatement valueBinder valueName span1 (monoScheme valueBinder) trueExpr,
        expressionStatement 2 (fixtureBoundVariableExpr valueBinder textInfo valueName)
      ]

missingImportProgram :: TypedProgram
missingImportProgram =
  typedProgram
  where
    fixture = "review-missing-import"
    modulePath = (fixtureModulePath fixture)
    typedProgram =
      TypedProgram
        Nothing
        [ typedModule
            modulePath
            relativeSource
            [TypedResolvedImport span1 ["Missing", "Library"] Nothing Nothing]
            []
            emptyInterface
            []
            boolInfo
        ]
        modulePath

candidateConstraintProgram :: TypedProgram
candidateConstraintProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface builtinMapDirectInfo modulePath)
  where
    fixture = "review-candidate-constraint"
    modulePath = (fixtureModulePath fixture)
    renderName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Render"
    equalName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    renderImpl = TypedImplId ["Prelude"] renderName [TypedTextType]
    equalImpl = TypedImplId ["Prelude"] equalName [TypedBoolType]
    constraint = TypedCapabilityConstraint (preludeCapability "Render") (Just "Render.map") TypedTextType
    equalCandidate = TypedEvidenceCandidate equalImpl (Just (TypedMethodId equalImpl "equal"))
    wrongMethodCandidate = TypedEvidenceCandidate renderImpl (Just (TypedMethodId renderImpl "render"))
    candidateExpression candidate =
      fixtureVariableExpr
        (TypedNodeInfo builtinMapType builtinMapRecipe [] [TypedEvidenceCandidates constraint [candidate]])
        (TypedBuiltinName "map")
    statements =
      [ expressionStatement 1 (candidateExpression equalCandidate),
        expressionStatement 2 (candidateExpression wrongMethodCandidate)
      ]

invalidVariableNamespaceName :: TypedCoreName
invalidVariableNamespaceName = resolved TypedCurrentModule TypedTypeNamespace "Flag"

invalidVariableNamespaceProgram :: TypedProgram
invalidVariableNamespaceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-variable-namespace"
    modulePath = (fixtureModulePath fixture)
    declaration =
      dataDeclarationWithNullaryConstructor
        modulePath
        [0, 0]
        invalidVariableNamespaceName
        []
    statements =
      [ TypedDataStatement declaration,
        expressionStatement 2 (fixtureVariableExpr boolInfo invalidVariableNamespaceName)
      ]

binderNameContractBinder :: TypedBinderId
binderNameContractBinder =
  binder
    (fixtureModulePath "review-binder-name-contract")
    [0]
    (resolved TypedCurrentModule TypedValueNamespace "embedded")

binderNameContractProgram :: TypedProgram
binderNameContractProgram =
  singleModuleProgram fixture relativeSource [] [statement] emptyInterface boolInfo modulePath
  where
    fixture = "review-binder-name-contract"
    modulePath = (fixtureModulePath fixture)
    publishedName = resolved TypedCurrentModule TypedValueNamespace "published"
    scheme = monoScheme binderNameContractBinder
    statement = TypedLetStatement binderNameContractBinder publishedName span1 scheme trueExpr

blockLocalGeneralizedSchemeProgram :: TypedProgram
blockLocalGeneralizedSchemeProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface boolInfo modulePath
  where
    fixture = "review-block-local-generalized-scheme"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "local"
    owner = binder modulePath [0, 0] valueName
    parameterId = TypedTypeParameterId 0
    scheme = fixtureScheme owner [parameterId] [] [] TypedBoolType TypedBoolRecipe
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] Nothing
    use = fixtureVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) valueName
    block = TypedBlockExpr boolInfo [TypedLetStatement owner valueName span1 scheme trueExpr, expressionStatement 2 use]

blockLocalMonomorphicSchemeProgram :: TypedProgram
blockLocalMonomorphicSchemeProgram =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 block] emptyInterface textInfo modulePath
  where
    fixture = "review-block-local-monomorphic-scheme"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "local"
    owner = binder modulePath [0, 0] valueName
    scheme = monoScheme owner
    use = fixtureBoundVariableExpr owner textInfo valueName
    block = TypedBlockExpr textInfo [TypedLetStatement owner valueName span1 scheme trueExpr, expressionStatement 2 use]

implMethodNameProgram :: TypedProgram
implMethodNameProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] [TypedImplStatement declaration] emptyInterface boolInfo modulePath)
  where
    fixture = "review-impl-method-name"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    publishedName = resolved TypedCurrentModule TypedValueNamespace "render"
    methodBinder = binder modulePath [0, 0] publishedName
    method = TypedMethodDefinition (TypedMethodId implId "equal") methodBinder publishedName span1 trueExpr
    declaration =
      TypedImplDeclaration
        span1
        implId
        [method, fixtureImplMethod modulePath [0, 1] implId "other"]

blockResultProgram :: TypedProgram
blockResultProgram =
  expressionFixtureProgram
    "review-block-result"
    (TypedBlockExpr boolInfo [expressionStatement 2 (literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "text"))])

nestedCasePatternPathProgram :: TypedProgram
nestedCasePatternPathProgram =
  expressionFixtureProgram fixture (TypedIfExpr boolInfo trueExpr nestedCase falseExpr)
  where
    fixture = "review-nested-case-pattern-path"
    nestedCase =
      TypedPatternCaseExpr
        boolInfo
        trueExpr
        [TypedCaseArm (TypedLiteralPattern boolInfo (TypedTextLiteral "wrong")) Nothing falseExpr]

operatorSchemeProgram :: TypedProgram
operatorSchemeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface textToTextInfo modulePath
  where
    fixture = "review-operator-scheme"
    modulePath = (fixtureModulePath fixture)
    operatorName = TypedGeneratedName (TypedOperatorBinding "$operator:%7E")
    owner = binder modulePath [0] operatorName
    operatorType = TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType TypedBoolType)
    operatorRecipe = TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe
    scheme = fixtureScheme owner [] [] [] operatorType operatorRecipe
    operator = TypedResolvedOperator operatorName "~"
    textExpr = literalExpr TypedTextType TypedManagedTextRecipe (TypedTextLiteral "text")
    textToTextInfo = info (TypedFunctionType TypedTextType TypedTextType) (TypedClosureRecipe [TypedManagedTextRecipe] TypedManagedTextRecipe)
    statements =
      [ TypedLetStatement owner operatorName span1 scheme (boolBinaryFunctionExpression modulePath [0]),
        expressionStatement 2 (TypedBinaryExpr textInfo operator textExpr textExpr),
        expressionStatement 3 (TypedLeftSectionExpr textToTextInfo textExpr operator),
        expressionStatement 4 (TypedRightSectionExpr textToTextInfo operator textExpr)
      ]

operatorSchemeFailures :: [TypedCoreValidationFailure]
operatorSchemeFailures =
  [ operatorFailure 1 TypedApplicationArgumentMismatch (TypedTypeDetail TypedBoolType TypedTextType),
    operatorFailure 1 TypedApplicationArgumentMismatch (TypedTypeDetail TypedBoolType TypedTextType),
    operatorFailure 1 TypedApplicationResultMismatch (TypedTypeDetail TypedBoolType TypedTextType),
    operatorFailure 2 TypedApplicationArgumentMismatch (TypedTypeDetail TypedBoolType TypedTextType),
    operatorFailure 2 TypedApplicationResultMismatch (TypedTypeDetail boolToBoolType (TypedFunctionType TypedTextType TypedTextType)),
    operatorFailure 2 TypedCallableShapeMismatch (TypedBinderDetail owner),
    operatorFailure 3 TypedApplicationArgumentMismatch (TypedTypeDetail TypedBoolType TypedTextType),
    operatorFailure 3 TypedApplicationResultMismatch (TypedTypeDetail boolToBoolType (TypedFunctionType TypedTextType TypedTextType)),
    operatorFailure 3 TypedCallableShapeMismatch (TypedBinderDetail owner)
  ]
  where
    owner =
      binder
        (fixtureModulePath "review-operator-scheme")
        [0]
        (TypedGeneratedName (TypedOperatorBinding "$operator:%7E"))
    operatorFailure statementIndex =
      TypedCoreValidationFailure (TypedExpressionPath (fixtureModulePath "review-operator-scheme") [statementIndex] [0])

selectiveImportProgram :: TypedProgram
selectiveImportProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "Selective")
    entryPath = (fixtureModulePath "review-selective-import")
    localName = resolved TypedCurrentModule TypedValueNamespace "identity"
    owner = binder libraryPath [0] localName
    scheme = monoScheme owner
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/Selective.jz")
        []
        [TypedModuleExport TypedValueNamespace "identity"]
        (TypedModuleInterface [TypedValueInterface localName scheme] [] [] [])
        [TypedLetStatement owner localName span1 scheme trueExpr]
        boolInfo
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing (Just ["missing"])]
        []
        emptyInterface
        []
        boolInfo

classParameterScopeProgram :: TypedProgram
classParameterScopeProgram =
  singleModuleProgram fixture relativeSource [] [TypedClassStatement declaration] emptyInterface boolInfo modulePath
  where
    fixture = "review-class-parameter-scope"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    methodName = resolved TypedCurrentModule TypedValueNamespace "equal"
    methodBinder = binder modulePath [0, 0] methodName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    methodType = TypedFunctionType parameterType (TypedFunctionType parameterType TypedBoolType)
    methodRecipe = TypedClosureRecipe [parameterRecipe, parameterRecipe] TypedBoolRecipe
    methodScheme = fixtureScheme methodBinder [] [] [] methodType methodRecipe
    declaration = TypedClassDeclaration span1 capabilityName [parameterId] [TypedMethodSignature methodName span1 methodScheme]

evidenceParameterContractProgram :: TypedProgram
evidenceParameterContractProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-evidence-parameter-contract"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    owner = binder modulePath [0] valueName
    parameterId = TypedTypeParameterId 0
    evidenceId = TypedEvidenceParameterId 0
    generalizedConstraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing (TypedTypeParameterType parameterId)
    scheme = fixtureScheme owner [parameterId] [TypedEvidenceParameter evidenceId generalizedConstraint] [] TypedBoolType TypedBoolRecipe
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] Nothing
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    selected selectedId constraint targetType =
      TypedSelectedEvidence
        ( TypedEvidenceUse
            (Just (TypedEvidenceParameterRef owner selectedId))
            constraint
            (TypedImplId ["Prelude"] capabilityName [targetType])
            Nothing
        )
    expression selection = fixtureVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] [selection]) valueName
    statements =
      [ TypedLetStatement owner valueName span1 scheme trueExpr,
        expressionStatement 2 (expression (selected (TypedEvidenceParameterId 7) (TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType) TypedBoolType)),
        expressionStatement 3 (expression (selected evidenceId (TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedCharType) TypedCharType))
      ]

invalidImplCapabilityName :: TypedCoreName
invalidImplCapabilityName = resolved TypedAmbientPrelude TypedValueNamespace "Equal"

invisibleImplCapabilityName :: TypedCoreName
invisibleImplCapabilityName = resolved (TypedImportedModule ["Hidden", "Capabilities"]) TypedCapabilityNamespace "Render"

implCapabilityNamespaceProgram :: TypedProgram
implCapabilityNamespaceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-impl-capability-namespace"
    modulePath = (fixtureModulePath fixture)
    statements =
      [ TypedImplStatement (TypedImplDeclaration span1 (TypedImplId modulePath invalidImplCapabilityName [TypedBoolType]) []),
        TypedImplStatement (TypedImplDeclaration span1 (TypedImplId modulePath invisibleImplCapabilityName [TypedBoolType]) [])
      ]

missingInstantiatedEvidenceProgram :: TypedProgram
missingInstantiatedEvidenceProgram =
  withFixturePrelude
    (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-missing-instantiated-evidence"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "same"
    owner = binder modulePath [0] valueName
    evidenceId = TypedEvidenceParameterId 0
    laterEvidenceId = TypedEvidenceParameterId 1
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    laterConstraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedCharType
    scheme = fixtureScheme owner [] [TypedEvidenceParameter evidenceId constraint, TypedEvidenceParameter laterEvidenceId laterConstraint] [] TypedBoolType TypedBoolRecipe
    instantiation = TypedInstantiation owner [] Nothing
    expression = fixtureVariableExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []) valueName
    statements = [TypedLetStatement owner valueName span1 scheme trueExpr, expressionStatement 2 expression]

constructorExpressionDataName :: TypedCoreName
constructorExpressionDataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"

constructorExpressionResultType :: TypedType
constructorExpressionResultType = TypedDataType constructorExpressionDataName []

constructorExpressionContractProgram :: TypedProgram
constructorExpressionContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-constructor-expression-contract"
    modulePath = (fixtureModulePath fixture)
    constructorName = resolved TypedCurrentModule TypedConstructorNamespace "Flag"
    declaration =
      TypedDataDeclaration
        span1
        constructorExpressionDataName
        []
        [ TypedConstructorDeclaration
            (binder modulePath [0, 0] constructorName)
            constructorName
            [TypedBoolType]
            [TypedBoolRecipe]
        ]
    statements = [TypedDataStatement declaration, expressionStatement 2 (fixtureBoundVariableExpr (binder modulePath [0, 0] constructorName) boolInfo constructorName)]

unrelatedTypeApplicationProgram :: TypedProgram
unrelatedTypeApplicationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface applicationInfo modulePath
  where
    fixture = "review-unrelated-type-application"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "unrelated"
    owner = binder modulePath [0] valueName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    functionType = TypedFunctionType parameterType parameterType
    functionRecipe = TypedClosureRecipe [parameterRecipe] parameterRecipe
    scheme =
      fixtureScheme
        owner
        [parameterId]
        []
        []
        functionType
        functionRecipe
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] (Just span1)
    applicationInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []
    expression = TypedTypeApplicationExpr applicationInfo trueExpr span1 TypedBoolType
    statements =
      [ TypedLetStatement owner valueName span1 scheme (polymorphicIdentityExpression modulePath [0] parameterId),
        expressionStatement 2 expression
      ]

lexicalBinderContractProgram :: TypedProgram
lexicalBinderContractProgram = expressionFixtureProgram fixture expression
  where
    fixture = "review-lexical-binder-contract"
    modulePath = (fixtureModulePath fixture)
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentBinder = binder modulePath [0] argumentName
    lambdaInfo =
      info
        (TypedFunctionType TypedBoolType TypedTextType)
        (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe)
    expression = TypedLambdaExpr lambdaInfo argumentBinder argumentName (fixtureBoundVariableExpr argumentBinder textInfo argumentName)

generalizedVariableContractProgram :: TypedProgram
generalizedVariableContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface badUseInfo modulePath
  where
    fixture = "review-generalized-variable-contract"
    modulePath = (fixtureModulePath fixture)
    valueName = resolved TypedCurrentModule TypedValueNamespace "identity"
    owner = binder modulePath [0] valueName
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    scheme =
      fixtureClosureScheme
        owner
        [parameterId]
        []
        []
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)
    instantiation = TypedInstantiation owner [TypedTypeArgument parameterId TypedBoolType] Nothing
    badUseInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] []
    expression = fixtureBoundVariableExpr owner badUseInfo valueName
    statements =
      [ TypedLetStatement owner valueName span1 scheme (polymorphicIdentityExpression modulePath [0] parameterId),
        expressionStatement 2 expression
      ]

enclosingInstantiationScopeProgram :: TypedProgram
enclosingInstantiationScopeProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-enclosing-instantiation-scope"
    modulePath = (fixtureModulePath fixture)
    identityName = resolved TypedCurrentModule TypedValueNamespace "identity"
    identityOwner = binder modulePath [0] identityName
    identityParameter = TypedTypeParameterId 0
    identityParameterType = TypedTypeParameterType identityParameter
    identityParameterRecipe = TypedRepresentationParameterRecipe identityParameter
    identityScheme =
      fixtureClosureScheme
        identityOwner
        [identityParameter]
        []
        []
        (TypedFunctionType identityParameterType identityParameterType)
        (TypedClosureRecipe [identityParameterRecipe] identityParameterRecipe)
    wrapperName = resolved TypedCurrentModule TypedValueNamespace "wrapper"
    wrapperOwner = binder modulePath [1] wrapperName
    wrapperParameter = TypedTypeParameterId 0
    wrapperParameterType = TypedTypeParameterType wrapperParameter
    wrapperParameterRecipe = TypedRepresentationParameterRecipe wrapperParameter
    wrapperType = TypedFunctionType wrapperParameterType wrapperParameterType
    wrapperRecipe = TypedClosureRecipe [wrapperParameterRecipe] wrapperParameterRecipe
    wrapperScheme = fixtureClosureScheme wrapperOwner [wrapperParameter] [] [] wrapperType wrapperRecipe
    instantiation = TypedInstantiation identityOwner [TypedTypeArgument identityParameter wrapperParameterType] Nothing
    expression = fixtureVariableExpr (TypedNodeInfo wrapperType wrapperRecipe [instantiation] []) identityName
    statements =
      [ TypedLetStatement
          identityOwner
          identityName
          span1
          identityScheme
          (polymorphicIdentityExpression modulePath [0] identityParameter),
        TypedLetStatement wrapperOwner wrapperName span1 wrapperScheme expression
      ]

implMethodContractProgram :: TypedProgram
implMethodContractProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-impl-method-contract"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Equal"
    parameterId = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    methodName = resolved TypedCurrentModule TypedValueNamespace "equal"
    methodType = TypedFunctionType parameterType (TypedFunctionType parameterType TypedBoolType)
    methodRecipe = TypedClosureRecipe [parameterRecipe, parameterRecipe] TypedBoolRecipe
    methodOwner = binder modulePath [0, 0] methodName
    methodScheme = fixtureScheme methodOwner [] [] [] methodType methodRecipe
    classDeclaration =
      TypedClassDeclaration
        span1
        capabilityName
        [parameterId]
        [TypedMethodSignature methodName span1 methodScheme]
    implId = TypedImplId modulePath capabilityName [TypedBoolType]
    implMethod =
      TypedMethodDefinition
        (TypedMethodId implId "equal")
        (binder modulePath [1, 0] methodName)
        methodName
        span1
        (TypedLiteralExpr textInfo (TypedTextLiteral "wrong"))
    statements =
      [ TypedClassStatement classDeclaration,
        TypedImplStatement (TypedImplDeclaration span1 implId [implMethod])
      ]

invalidDataDeclarationName :: TypedCoreName
invalidDataDeclarationName = resolved TypedCurrentModule TypedValueNamespace "Flag"

dataDeclarationNamespaceProgram :: TypedProgram
dataDeclarationNamespaceProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-data-declaration-namespace"
    modulePath = (fixtureModulePath fixture)
    statements =
      [ TypedDataStatement
          ( dataDeclarationWithNullaryConstructor
              modulePath
              [0, 0]
              invalidDataDeclarationName
              []
          )
      ]

duplicateDeclarationName :: TypedCoreName
duplicateDeclarationName = resolved TypedCurrentModule TypedValueNamespace "duplicate"

duplicateDeclarationProgram :: TypedProgram
duplicateDeclarationProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-duplicate-declaration"
    modulePath = (fixtureModulePath fixture)
    firstOwner = binder modulePath [0] duplicateDeclarationName
    secondOwner = binder modulePath [1] duplicateDeclarationName
    statements =
      [ TypedSignatureStatement firstOwner duplicateDeclarationName span1 (monoScheme firstOwner),
        TypedSignatureStatement secondOwner duplicateDeclarationName span1 (monoScheme secondOwner)
      ]

importedImplQualificationProgram :: TypedProgram
importedImplQualificationProgram = TypedProgram Nothing [libraryModule, entryModule] entryPath
  where
    libraryPath = (fixtureLibraryPath "QualifiedImpl")
    entryPath = (fixtureModulePath "review-imported-impl-qualification")
    localDataName = resolved TypedCurrentModule TypedTypeNamespace "Flag"
    localCapabilityName = resolved TypedCurrentModule TypedCapabilityNamespace "Mark"
    parameterId = TypedTypeParameterId 0
    dataDeclaration =
      dataDeclarationWithNullaryConstructor
        libraryPath
        [0, 0]
        localDataName
        []
    classDeclaration = TypedClassDeclaration span1 localCapabilityName [parameterId] []
    localImplId = TypedImplId libraryPath localCapabilityName [TypedDataType localDataName []]
    libraryInterface =
      TypedModuleInterface
        []
        [TypedDataInterface dataDeclaration]
        [TypedClassInterface classDeclaration]
        [TypedImplInterface localImplId]
    libraryModule =
      typedModule
        libraryPath
        (TypedSourcePath "src/Library/QualifiedImpl.jz")
        []
        [ TypedModuleExport TypedTypeNamespace "Flag",
          TypedModuleExport TypedCapabilityNamespace "Mark"
        ]
        libraryInterface
        [ TypedDataStatement dataDeclaration,
          TypedClassStatement classDeclaration,
          TypedImplStatement (TypedImplDeclaration span1 localImplId [])
        ]
        boolInfo
    importedDataName = resolved (TypedImportedModule libraryPath) TypedTypeNamespace "Flag"
    importedCapabilityName = resolved (TypedImportedModule libraryPath) TypedCapabilityNamespace "Mark"
    importedTargetType = TypedDataType importedDataName []
    importedImplId = TypedImplId libraryPath importedCapabilityName [importedTargetType]
    constraint = TypedCapabilityConstraint importedCapabilityName Nothing importedTargetType
    valueName = resolved TypedCurrentModule TypedValueNamespace "usesMark"
    valueOwner = binder entryPath [0] valueName
    evidenceParameter = TypedEvidenceParameterId 0
    valueScheme =
      fixtureScheme
        valueOwner
        []
        [TypedEvidenceParameter evidenceParameter constraint]
        []
        TypedBoolType
        TypedBoolRecipe
    instantiation = TypedInstantiation valueOwner [] Nothing
    evidence =
      TypedSelectedEvidence
        ( TypedEvidenceUse
            (Just (TypedEvidenceParameterRef valueOwner evidenceParameter))
            constraint
            importedImplId
            Nothing
        )
    expression =
      fixtureVariableExpr
        (TypedNodeInfo TypedBoolType TypedBoolRecipe [instantiation] [evidence])
        valueName
    entryModule =
      typedModule
        entryPath
        relativeSource
        [TypedResolvedImport span1 libraryPath Nothing Nothing]
        []
        emptyInterface
        [ TypedLetStatement valueOwner valueName span1 valueScheme trueExpr,
          expressionStatement 1 expression
        ]
        (expressionInfoForFixture expression)

implTargetArityProgram :: TypedProgram
implTargetArityProgram =
  withFixturePrelude (singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath)
  where
    fixture = "review-impl-target-arity"
    modulePath = (fixtureModulePath fixture)
    capabilityName = resolved TypedAmbientPrelude TypedCapabilityNamespace "Equal"
    implId = TypedImplId modulePath capabilityName [TypedBoolType, TypedCharType]
    constraint = TypedCapabilityConstraint (preludeCapability "Equal") Nothing TypedBoolType
    evidence = TypedSelectedEvidence (TypedEvidenceUse Nothing constraint implId Nothing)
    expression = TypedLiteralExpr (TypedNodeInfo TypedBoolType TypedBoolRecipe [] [evidence]) (TypedBooleanLiteral True)
    statements =
      [ TypedImplStatement (TypedImplDeclaration span1 implId []),
        expressionStatement 2 expression
      ]

localDeclarationOriginBinder :: TypedBinderId
localDeclarationOriginBinder =
  binder
    (fixtureModulePath "review-local-declaration-origin")
    [0]
    localDeclarationOriginName

localDeclarationOriginName :: TypedCoreName
localDeclarationOriginName = resolved (TypedImportedModule ["Other", "Module"]) TypedValueNamespace "foreign"

localDeclarationOriginProgram :: TypedProgram
localDeclarationOriginProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-local-declaration-origin"
    modulePath = (fixtureModulePath fixture)
    scheme = monoScheme localDeclarationOriginBinder
    statements =
      [ TypedLetStatement
          localDeclarationOriginBinder
          localDeclarationOriginName
          span1
          scheme
          trueExpr
      ]

reservedValueIdentifierName :: TypedCoreName
reservedValueIdentifierName =
  resolved TypedCurrentModule TypedValueNamespace "value"

reservedValueIdentifierProgram :: TypedProgram
reservedValueIdentifierProgram =
  singleModuleProgram fixture relativeSource [] statements emptyInterface boolInfo modulePath
  where
    fixture = "review-reserved-value-identifier"
    modulePath = fixtureModulePath fixture
    valueBinder = binder modulePath [0] reservedValueIdentifierName
    statements =
      [ TypedLetStatement
          valueBinder
          reservedValueIdentifierName
          span1
          (monoScheme valueBinder)
          trueExpr
      ]

reservedValueModulePath :: [Text]
reservedValueModulePath = ["Fixture", "value"]

reservedValueModulePathProgram :: TypedProgram
reservedValueModulePathProgram =
  TypedProgram
    Nothing
    [ typedModule
        reservedValueModulePath
        relativeSource
        []
        []
        emptyInterface
        [expressionStatement 1 trueExpr]
        boolInfo
    ]
    reservedValueModulePath

expressionFixtureProgram :: Text -> TypedExpr -> TypedProgram
expressionFixtureProgram fixture expression =
  singleModuleProgram fixture relativeSource [] [expressionStatement 1 expression] emptyInterface (expressionInfoForFixture expression) (fixtureModulePath fixture)

expressionInfoForFixture :: TypedExpr -> TypedNodeInfo
expressionInfoForFixture expression =
  case expression of
    TypedLiteralExpr valueInfo _ -> valueInfo
    TypedVariableExpr valueInfo _ _ -> valueInfo
    TypedLambdaExpr valueInfo _ _ _ -> valueInfo
    TypedOperatorValueExpr valueInfo _ -> valueInfo
    TypedListExpr valueInfo _ -> valueInfo
    TypedTupleExpr valueInfo _ -> valueInfo
    TypedApplyExpr valueInfo _ _ -> valueInfo
    TypedTypeApplicationExpr valueInfo _ _ _ -> valueInfo
    TypedIfExpr valueInfo _ _ _ -> valueInfo
    TypedPatternCaseExpr valueInfo _ _ -> valueInfo
    TypedBinaryExpr valueInfo _ _ _ -> valueInfo
    TypedLeftSectionExpr valueInfo _ _ -> valueInfo
    TypedRightSectionExpr valueInfo _ _ -> valueInfo
    TypedBlockExpr valueInfo _ -> valueInfo

emptyInterface :: TypedModuleInterface
emptyInterface = TypedModuleInterface [] [] [] []

span1 :: TypedSpan
span1 = TypedSpan 1 1

expressionStatement :: Int -> TypedExpr -> TypedStatement
expressionStatement line expression = TypedExpressionStatement (TypedSpan line 1) expression

literalExpr :: TypedType -> TypedRepresentationRecipe -> TypedLiteral -> TypedExpr
literalExpr typeValue recipe literal = TypedLiteralExpr (info typeValue recipe) literal

info :: TypedType -> TypedRepresentationRecipe -> TypedNodeInfo
info typeValue recipe = TypedNodeInfo typeValue recipe [] []

boolInfo :: TypedNodeInfo
boolInfo = info TypedBoolType TypedBoolRecipe

textInfo :: TypedNodeInfo
textInfo = info TypedTextType TypedManagedTextRecipe

unitInfo :: TypedNodeInfo
unitInfo = info (TypedTupleType []) TypedUnitRecipe

pairInfo :: TypedNodeInfo
pairInfo = info (TypedTupleType [TypedBoolType, TypedBoolType]) (TypedManagedProductRecipe [TypedBoolRecipe, TypedBoolRecipe])

boolListInfo :: TypedNodeInfo
boolListInfo = info (TypedListType TypedBoolType) (TypedManagedListRecipe TypedBoolRecipe)

trueExpr :: TypedExpr
trueExpr = TypedLiteralExpr boolInfo (TypedBooleanLiteral True)

falseExpr :: TypedExpr
falseExpr = TypedLiteralExpr boolInfo (TypedBooleanLiteral False)

resolved :: TypedNameOrigin -> TypedNameNamespace -> Text -> TypedCoreName
resolved = TypedResolvedName

binder :: [Text] -> [Int] -> TypedCoreName -> TypedBinderId
binder modulePath lexicalPath name = TypedBinderId (modulePath, lexicalPath, name)

dataDeclarationWithNullaryConstructor :: [Text] -> [Int] -> TypedCoreName -> [TypedTypeParameterId] -> TypedDataDeclaration
dataDeclarationWithNullaryConstructor modulePath lexicalPath dataName parameters =
  TypedDataDeclaration
    span1
    dataName
    parameters
    [ TypedConstructorDeclaration
        (binder modulePath lexicalPath constructorName)
        constructorName
        []
        []
    ]
  where
    constructorName =
      case dataName of
        TypedResolvedName origin _ identifier ->
          TypedResolvedName origin TypedConstructorNamespace identifier
        other -> other

signatureProgram :: Text -> TypedBinderId -> TypedCoreName -> TypedScheme -> TypedProgram
signatureProgram fixture valueBinder valueName scheme =
  singleModuleProgram
    fixture
    relativeSource
    []
    [TypedSignatureStatement valueBinder valueName span1 scheme]
    emptyInterface
    boolInfo
    (fixtureModulePath fixture)

singleModuleProgram :: Text -> TypedSourcePath -> [TypedModuleExport] -> [TypedStatement] -> TypedModuleInterface -> TypedNodeInfo -> [Text] -> TypedProgram
singleModuleProgram fixture sourcePath exports statements interface moduleInfo entryModule =
  TypedProgram
    Nothing
    [typedModule (fixtureModulePath fixture) sourcePath [] exports interface statements moduleInfo]
    entryModule

typedModule :: [Text] -> TypedSourcePath -> [TypedResolvedImport] -> [TypedModuleExport] -> TypedModuleInterface -> [TypedStatement] -> TypedNodeInfo -> TypedModule
typedModule modulePath sourcePath imports exports interface statements moduleInfo =
  TypedModule
    modulePath
    sourcePath
    imports
    exports
    interface
    []
    statements
    (if hasTerminalExpression statements then moduleInfo else unitInfo)

hasTerminalExpression :: [TypedStatement] -> Bool
hasTerminalExpression statements =
  case reverse statements of
    TypedExpressionStatement {} : _ -> True
    _ -> False

polymorphicIdentityExpression :: [Text] -> [Int] -> TypedTypeParameterId -> TypedExpr
polymorphicIdentityExpression modulePath lexicalPath parameterId =
  TypedLambdaExpr
    functionInfo
    (binder modulePath (lexicalPath <> [0]) argumentName)
    argumentName
    (fixtureBoundVariableExpr argumentBinder parameterInfo argumentName)
  where
    argumentName = resolved TypedCurrentModule TypedValueNamespace "argument"
    argumentBinder = binder modulePath (lexicalPath <> [0]) argumentName
    parameterType = TypedTypeParameterType parameterId
    parameterRecipe = TypedRepresentationParameterRecipe parameterId
    parameterInfo = info parameterType parameterRecipe
    functionInfo =
      info
        (TypedFunctionType parameterType parameterType)
        (TypedClosureRecipe [parameterRecipe] parameterRecipe)

boolBinaryFunctionExpression :: [Text] -> [Int] -> TypedExpr
boolBinaryFunctionExpression modulePath lexicalPath =
  TypedLambdaExpr
    binaryInfo
    (binder modulePath (lexicalPath <> [0]) leftName)
    leftName
    ( TypedLambdaExpr
        boolToBoolInfo
        (binder modulePath (lexicalPath <> [0, 0]) rightName)
        rightName
        trueExpr
    )
  where
    leftName = resolved TypedCurrentModule TypedValueNamespace "left"
    rightName = resolved TypedCurrentModule TypedValueNamespace "right"
    binaryInfo =
      info
        (TypedFunctionType TypedBoolType boolToBoolType)
        (TypedClosureRecipe [TypedBoolRecipe, TypedBoolRecipe] TypedBoolRecipe)

relativeSource :: TypedSourcePath
relativeSource = TypedSourcePath "src/Fixture/Main.jz"

fixtureValueName :: Text -> TypedCoreName
fixtureValueName = resolved TypedCurrentModule TypedValueNamespace

preludeCapability :: Text -> TypedCoreName
preludeCapability = resolved TypedAmbientPrelude TypedCapabilityNamespace

fixtureBinder :: Text -> Int -> TypedCoreName -> TypedBinderId
fixtureBinder fixture lexicalIndex = binder (fixtureModulePath fixture) [lexicalIndex]

fixtureModulePath :: Text -> [Text]
fixtureModulePath fixture = ["Fixture", fixtureModuleSegment fixture]

fixtureLibraryPath :: Text -> [Text]
fixtureLibraryPath fixture = ["Library", fixtureModuleSegment fixture]

fixtureModuleSegment :: Text -> Text
fixtureModuleSegment = Text.replace "-" "_"

monoScheme :: TypedBinderId -> TypedScheme
monoScheme valueBinder = fixtureScheme valueBinder [] [] [] TypedBoolType TypedBoolRecipe

fixtureScheme :: TypedBinderId -> [TypedTypeParameterId] -> [TypedEvidenceParameter] -> [TypedPrimitiveConstraint] -> TypedType -> TypedRepresentationRecipe -> TypedScheme
fixtureScheme owner parameters evidence primitive typeValue recipe =
  TypedScheme owner parameters evidence primitive typeValue recipe callableShape
  where
    callableShape =
      case typeValue of
        TypedFunctionType {} -> Just TypedDirectCallableShape
        _ -> Nothing

fixtureClosureScheme :: TypedBinderId -> [TypedTypeParameterId] -> [TypedEvidenceParameter] -> [TypedPrimitiveConstraint] -> TypedType -> TypedRepresentationRecipe -> TypedScheme
fixtureClosureScheme owner parameters evidence primitive typeValue recipe =
  TypedScheme owner parameters evidence primitive typeValue recipe callableShape
  where
    callableShape =
      case typeValue of
        TypedFunctionType {} -> Just TypedClosureCallableShape
        _ -> Nothing

fixtureVariableExpr :: TypedNodeInfo -> TypedCoreName -> TypedExpr
fixtureVariableExpr nodeInfo name = TypedVariableExpr nodeInfo name binderReference
  where
    binderReference =
      case nodeInstantiationsForFixture nodeInfo of
        TypedInstantiation owner _ _ : _ -> Just owner
        [] -> Nothing

fixtureBoundVariableExpr :: TypedBinderId -> TypedNodeInfo -> TypedCoreName -> TypedExpr
fixtureBoundVariableExpr owner nodeInfo name = TypedVariableExpr nodeInfo name (Just owner)

nodeInstantiationsForFixture :: TypedNodeInfo -> [TypedInstantiation]
nodeInstantiationsForFixture (TypedNodeInfo _ _ instantiations _) = instantiations

boolToBoolType :: TypedType
boolToBoolType = TypedFunctionType TypedBoolType TypedBoolType

boolToBoolRecipe :: TypedRepresentationRecipe
boolToBoolRecipe = TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe

boolToBoolInfo :: TypedNodeInfo
boolToBoolInfo = info boolToBoolType boolToBoolRecipe

builtinMapType :: TypedType
builtinMapType =
  TypedFunctionType
    (TypedFunctionType TypedBoolType TypedTextType)
    (TypedFunctionType (TypedListType TypedBoolType) (TypedListType TypedTextType))

builtinMapRecipe :: TypedRepresentationRecipe
builtinMapRecipe =
  TypedClosureRecipe
    [ TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe,
      TypedManagedListRecipe TypedBoolRecipe
    ]
    (TypedManagedListRecipe TypedManagedTextRecipe)

builtinMapInfo :: TypedNodeInfo
builtinMapInfo = info builtinMapType builtinMapValueRecipe

builtinMapDirectInfo :: TypedNodeInfo
builtinMapDirectInfo = info builtinMapType builtinMapRecipe

builtinMapDirectExpression :: [Text] -> [Int] -> TypedExpr
builtinMapDirectExpression modulePath lexicalPath =
  TypedLambdaExpr
    builtinMapDirectInfo
    mapperBinder
    mapperName
    ( TypedLambdaExpr
        remainingInfo
        valuesBinder
        valuesName
        (TypedApplyExpr resultInfo partialApplication valuesReference)
    )
  where
    mapperName = resolved TypedCurrentModule TypedValueNamespace "mapper"
    mapperBinder = binder modulePath (lexicalPath <> [0]) mapperName
    mapperInfo =
      info
        (TypedFunctionType TypedBoolType TypedTextType)
        (TypedClosureRecipe [TypedBoolRecipe] TypedManagedTextRecipe)
    mapperReference = fixtureBoundVariableExpr mapperBinder mapperInfo mapperName
    valuesName = resolved TypedCurrentModule TypedValueNamespace "values"
    valuesBinder = binder modulePath (lexicalPath <> [0, 0]) valuesName
    valuesInfo = info (TypedListType TypedBoolType) (TypedManagedListRecipe TypedBoolRecipe)
    valuesReference = fixtureBoundVariableExpr valuesBinder valuesInfo valuesName
    remainingInfo =
      info
        (TypedFunctionType (TypedListType TypedBoolType) (TypedListType TypedTextType))
        (TypedClosureRecipe [TypedManagedListRecipe TypedBoolRecipe] (TypedManagedListRecipe TypedManagedTextRecipe))
    resultInfo = info (TypedListType TypedTextType) (TypedManagedListRecipe TypedManagedTextRecipe)
    partialApplication =
      TypedApplyExpr
        remainingInfo
        (fixtureVariableExpr builtinMapDirectInfo (TypedBuiltinName "map"))
        mapperReference

moduleFailure :: Text -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
moduleFailure fixture = TypedCoreValidationFailure (TypedModulePath (fixtureModulePath fixture))

statementFailure :: Text -> Int -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
statementFailure fixture statementIndex = statementFailureAtPath fixture [statementIndex]

statementFailureAtPath :: Text -> [Int] -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
statementFailureAtPath fixture statementPath = TypedCoreValidationFailure (TypedStatementPath (fixtureModulePath fixture) statementPath)

expressionFailure :: Text -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
expressionFailure fixture = expressionFailureAt fixture 0

expressionFailureAt :: Text -> Int -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
expressionFailureAt fixture statementIndex = expressionFailureAtPath fixture [statementIndex]

expressionFailureAtPath :: Text -> [Int] -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
expressionFailureAtPath fixture statementPath =
  TypedCoreValidationFailure (TypedExpressionPath (fixtureModulePath fixture) statementPath [0])

patternFailure :: Text -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
patternFailure fixture =
  TypedCoreValidationFailure (TypedPatternPath (fixtureModulePath fixture) [0] [0, 0])

instantiationProgram :: Text -> Maybe TypedSpan -> TypedProgram
instantiationProgram fixture explicitSpan =
  programWith
    fixture
    [ TypedLetStatement
        owner
        name
        span1
        scheme
        (polymorphicIdentityExpression (fixtureModulePath fixture) [0] parameterId),
      expressionStatement 2 expression
    ]
    emptyInterface
    instantiatedInfo
  where
    name = resolved TypedCurrentModule TypedValueNamespace "identity"
    owner = binder (fixtureModulePath fixture) [0] name
    instantiation =
      TypedInstantiation
        owner
        [TypedTypeArgument (TypedTypeParameterId 0) TypedBoolType]
        explicitSpan
    parameterId = TypedTypeParameterId 0
    scheme =
      fixtureClosureScheme
        owner
        [parameterId]
        []
        []
        (TypedFunctionType (TypedTypeParameterType parameterId) (TypedTypeParameterType parameterId))
        (TypedClosureRecipe [TypedRepresentationParameterRecipe parameterId] (TypedRepresentationParameterRecipe parameterId))
    instantiatedInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType TypedBoolType)
        (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe)
        [instantiation]
        []
    expression =
      case explicitSpan of
        Nothing -> fixtureVariableExpr instantiatedInfo name
        Just explicitApplicationSpan ->
          TypedTypeApplicationExpr
            instantiatedInfo
            (fixtureVariableExpr instantiatedInfo name)
            explicitApplicationSpan
            TypedBoolType

programWith :: Text -> [TypedStatement] -> TypedModuleInterface -> TypedNodeInfo -> TypedProgram
programWith fixtureName statements interface moduleInfo =
  TypedProgram
    Nothing
    [ typedModule
        (fixtureModulePath fixtureName)
        (TypedSourcePath ("src/Fixture/" <> fixtureName <> ".jz"))
        []
        []
        interface
        statements
        moduleInfo
    ]
    (fixtureModulePath fixtureName)

