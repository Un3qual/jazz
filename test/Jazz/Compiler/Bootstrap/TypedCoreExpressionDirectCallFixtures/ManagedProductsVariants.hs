{-# LANGUAGE OverloadedStrings #-}

-- | Source fixtures for the managed product and local-variant profile.
module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.ManagedProductsVariants where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Source
import Jazz.Compiler.TypedCore

managedProductVariantFixtures :: [(Text, Fixture)]
managedProductVariantFixtures =
  [ ("managed-tuple", sourceFixtureNoExports "managed-tuple" managedTupleSource),
    ("managed-option", sourceFixtureNoExports "managed-option" managedOptionSource),
    ("managed-exported-option", sourceFixture "managed-exported-option" managedExportedOptionSource),
    ("managed-tree", sourceFixtureNoExports "managed-tree" managedTreeSource),
    ( "managed-tuple-child-failure",
      sourceFixtureNoExports "managed-tuple-child-failure" retainedTupleChildFailureSource
    ),
    ( "managed-data-sibling-failure",
      sourceFixtureNoExports "managed-data-sibling-failure" retainedDataSiblingFailureSource
    ),
    ( "managed-bare-constructor-failure",
      sourceFixtureNoExports "managed-bare-constructor-failure" bareConstructorSource
    ),
    ( "managed-partial-constructor-failure",
      sourceFixtureNoExports "managed-partial-constructor-failure" partialConstructorSource
    ),
    ( "managed-list-field-failure",
      sourceFixtureNoExports "managed-list-field-failure" listFieldSource
    ),
    ( "managed-unresolved-constructor-failure",
      sourceFixtureNoExports "managed-unresolved-constructor-failure" unresolvedConstructorSource
    )
  ]

managedProductVariantFixture :: Text -> Fixture
managedProductVariantFixture name =
  case lookup name managedProductVariantFixtures of
    Just fixture -> fixture
    Nothing -> error "managed product/variant fixture is missing"

managedProductVariantExpectedPrograms :: [(Text, TypedProgram)]
managedProductVariantExpectedPrograms =
  [ ("managed-tuple", managedTupleProgram),
    ("managed-option", managedOptionProgram),
    ("managed-exported-option", managedExportedOptionProgram),
    ("managed-tree", managedTreeProgram)
  ]

managedProductVariantManifestExpectedPrograms :: [(Text, TypedProgram)]
managedProductVariantManifestExpectedPrograms =
  [ ("non-unit-tuple", manifestTupleProgram),
    ("data-value", manifestDataProgram)
  ]

manifestTupleProgram :: TypedProgram
manifestTupleProgram =
  managedProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        (TypedTupleExpr tupleInfo [intExpr 1, intExpr 2])
    ]
    tupleInfo
  where
    tupleInfo =
      TypedNodeInfo
        (TypedTupleType [TypedIntType, TypedIntType])
        (TypedManagedProductRecipe [TypedSignedIntegerRecipe 64, TypedSignedIntegerRecipe 64])
        []
        []

manifestDataProgram :: TypedProgram
manifestDataProgram =
  managedProgram
    [ TypedDataStatement declaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        (TypedVariableExpr boxInfo boxName (Just boxBinder))
    ]
    boxInfo
  where
    dataName = typeName "Box"
    boxName = constructorName "Box"
    boxBinder = constructorBinder 0 boxName
    declaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        dataName
        []
        [TypedConstructorDeclaration boxBinder boxName [] []]
    boxInfo = variantInfo dataName []

managedTupleProgram :: TypedProgram
managedTupleProgram =
  managedProgram
    [ TypedExpressionStatement
        (TypedSpan 2 1)
        (TypedTupleExpr tupleInfo [intExpr 1, textExpr "two"])
    ]
    tupleInfo
  where
    tupleInfo =
      TypedNodeInfo
        (TypedTupleType [TypedIntType, TypedTextType])
        (TypedManagedProductRecipe [TypedSignedIntegerRecipe 64, TypedManagedTextRecipe])
        []
        []

managedOptionProgram :: TypedProgram
managedOptionProgram = optionProgram [] (TypedModuleInterface [] [] [] [])

managedExportedOptionProgram :: TypedProgram
managedExportedOptionProgram =
  optionProgram
    [ TypedModuleExport TypedTypeNamespace "Option",
      TypedModuleExport TypedConstructorNamespace "None",
      TypedModuleExport TypedConstructorNamespace "Some"
    ]
    (TypedModuleInterface [] [TypedDataInterface optionDeclaration] [] [])

optionProgram :: [TypedModuleExport] -> TypedModuleInterface -> TypedProgram
optionProgram exports interface =
  managedProgramWithInterface
    exports
    interface
    [ TypedDataStatement optionDeclaration,
      TypedExpressionStatement
        (TypedSpan 3 1)
        (constructorCall someBinder someName optionIntInfo [intInfo] [intExpr 7])
    ]
    optionIntInfo

optionParameter :: TypedTypeParameterId
optionParameter = TypedTypeParameterId 0

optionName, noneName, someName :: TypedCoreName
optionName = typeName "Option"
noneName = constructorName "None"
someName = constructorName "Some"

noneBinder, someBinder :: TypedBinderId
noneBinder = constructorBinder 0 noneName
someBinder = constructorBinder 1 someName

optionDeclaration :: TypedDataDeclaration
optionDeclaration =
  TypedDataDeclaration
    (TypedSpan 2 1)
    optionName
    [optionParameter]
    [ TypedConstructorDeclaration noneBinder noneName [] [],
      TypedConstructorDeclaration
        someBinder
        someName
        [TypedTypeParameterType optionParameter]
        [TypedRepresentationParameterRecipe optionParameter]
    ]

optionIntInfo :: TypedNodeInfo
optionIntInfo = variantInfo optionName [TypedIntType]

managedTreeProgram :: TypedProgram
managedTreeProgram =
  managedProgram
    [ TypedDataStatement treeDeclaration,
      TypedExpressionStatement (TypedSpan 3 1) branchExpression
    ]
    treeIntInfo
  where
    parameter = TypedTypeParameterId 0
    parameterType = TypedTypeParameterType parameter
    treeName = typeName "Tree"
    leafName = constructorName "Leaf"
    branchName = constructorName "Branch"
    leafBinder = constructorBinder 0 leafName
    branchBinder = constructorBinder 1 branchName
    genericTreeType = TypedDataType treeName [parameterType]
    genericTreeRecipe = TypedManagedVariantRecipe treeName [parameterType]
    treeDeclaration =
      TypedDataDeclaration
        (TypedSpan 2 1)
        treeName
        [parameter]
        [ TypedConstructorDeclaration
            leafBinder
            leafName
            [parameterType]
            [TypedRepresentationParameterRecipe parameter],
          TypedConstructorDeclaration
            branchBinder
            branchName
            [genericTreeType, genericTreeType]
            [genericTreeRecipe, genericTreeRecipe]
        ]
    treeIntInfo = variantInfo treeName [TypedIntType]
    leaf value = constructorCall leafBinder leafName treeIntInfo [intInfo] [intExpr value]
    branchExpression =
      constructorCall
        branchBinder
        branchName
        treeIntInfo
        [treeIntInfo, treeIntInfo]
        [leaf 1, leaf 2]

managedProgram :: [TypedStatement] -> TypedNodeInfo -> TypedProgram
managedProgram statements moduleInfo =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        statements
        moduleInfo
    ]
    modulePath

managedProgramWithInterface :: [TypedModuleExport] -> TypedModuleInterface -> [TypedStatement] -> TypedNodeInfo -> TypedProgram
managedProgramWithInterface exports interface statements moduleInfo =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        exports
        interface
        []
        statements
        moduleInfo
    ]
    modulePath

constructorCall :: TypedBinderId -> TypedCoreName -> TypedNodeInfo -> [TypedNodeInfo] -> [TypedExpr] -> TypedExpr
constructorCall owner name resultInfo fieldInfos arguments =
  case fieldInfos of
    [] -> TypedVariableExpr instantiatedResultInfo name (Just owner)
    _ -> saturated constructorExpression fieldInfos arguments
  where
    typeArguments =
      case typedExpressionType resultInfo of
        TypedDataType _ argumentsValue ->
          zipWith TypedTypeArgument [TypedTypeParameterId index | index <- [0 ..]] argumentsValue
        _ -> []
    instantiation = TypedInstantiation owner typeArguments Nothing
    instantiatedResultInfo = addInstantiation resultInfo instantiation
    constructorInfo =
      TypedNodeInfo
        (foldr (TypedFunctionType . typedExpressionType) (typedExpressionType resultInfo) fieldInfos)
        (TypedClosureRecipe (map typedExpressionRecipe fieldInfos) (typedExpressionRecipe resultInfo))
        [instantiation]
        []
    constructorExpression = TypedVariableExpr constructorInfo name (Just owner)

    saturated function remainingFields remainingArguments =
      case (remainingFields, remainingArguments) of
        (_ : fieldRest, argument : argumentRest) ->
          let applicationInfo =
                case fieldRest of
                  [] -> resultInfo
                  _ ->
                    TypedNodeInfo
                      (foldr (TypedFunctionType . typedExpressionType) (typedExpressionType resultInfo) fieldRest)
                      (TypedClosureRecipe (map typedExpressionRecipe fieldRest) (typedExpressionRecipe resultInfo))
                      []
                      []
           in saturated (TypedApplyExpr applicationInfo function argument) fieldRest argumentRest
        ([], []) -> function
        _ -> error "constructor fixture must be exactly saturated"

addInstantiation :: TypedNodeInfo -> TypedInstantiation -> TypedNodeInfo
addInstantiation (TypedNodeInfo typeValue recipe _ evidence) instantiation =
  TypedNodeInfo typeValue recipe [instantiation] evidence

variantInfo :: TypedCoreName -> [TypedType] -> TypedNodeInfo
variantInfo name arguments =
  TypedNodeInfo
    (TypedDataType name arguments)
    (TypedManagedVariantRecipe name arguments)
    []
    []

typeName :: Text -> TypedCoreName
typeName = TypedResolvedName TypedCurrentModule TypedTypeNamespace

constructorName :: Text -> TypedCoreName
constructorName = TypedResolvedName TypedCurrentModule TypedConstructorNamespace

constructorBinder :: Int -> TypedCoreName -> TypedBinderId
constructorBinder constructorIndex name = TypedBinderId (modulePath, [0, constructorIndex], name)

managedExportedOptionSource :: Text
managedExportedOptionSource =
  Text.unlines
    [ "module App::Main (type Option(..)) {",
      "data Option a = None | Some a.",
      "Some 7.",
      "}"
    ]

bareConstructorSource, partialConstructorSource, listFieldSource, unresolvedConstructorSource :: Text
bareConstructorSource = Text.unlines ["data Box = Box Int.", "Box."]
partialConstructorSource = Text.unlines ["data Pair a b = Pair a b.", "Pair 1."]
listFieldSource = Text.unlines ["data Box = Box List(Int).", "Box [1]."]
unresolvedConstructorSource = Text.unlines ["data Option a = None | Some a.", "None."]
