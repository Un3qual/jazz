{-# LANGUAGE OverloadedStrings #-}

-- | Independent lowerer profile and structural boundary artifacts.
module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.LowererBoundary where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.CallsCaptures
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.ManagedText
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Scalar
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Source
import Jazz.Compiler.TypedCore

lowererBoundaryPrograms :: [(Text, TypedProgram)]
lowererBoundaryPrograms =
  [ ("scalar-binding-unsupported-rhs", invalidScalarBindingRhsProgram),
    ("combined-statement-failure-order", combinedStatementFailureOrderLowererProgram),
    ("recursion-descendant-failure-order", recursionDescendantFailureOrderLowererProgram),
    ("interleaved-capture-mutual-recursion", interleavedCaptureMutualRecursiveLowererProgram),
    ("closure-valued-parameter", closureValuedParameterLowererProgram),
    ("closure-valued-result", closureValuedResultLowererProgram),
    ("closure-shaped-named-function", closureShapeLowererProgram),
    ("closure-shaped-named-application", closureShapeApplicationLowererProgram),
    ("callable-parameter-shadows-top-level-lowerer", callableParameterShadowsTopLevelLowererProgram),
    ("callable-parameter-value-shadows-enclosing-function-lowerer", callableParameterValueShadowsEnclosingFunctionLowererProgram),
    ("non-concrete-closure-representation", nonConcreteClosureRepresentationLowererProgram),
    ("duplicate-parameter-function", duplicateParameterLowererProgram),
    ("self-recursive-duplicate-parameter-function", selfRecursiveDuplicateParameterLowererProgram),
    ("duplicate-function-identity", duplicateFunctionLowererProgram),
    ("capturing-function", capturingLowererProgram),
    ("closure-shaped-self-recursive-function", closureShapedSelfRecursiveLowererProgram),
    ("nested-lambda-closure-value-self-recursion", nestedLambdaClosureValueSelfRecursiveLowererProgram),
    ("imported-direct-call", importedDirectCallLowererProgram)
  ]

validIndependentLowererPrograms :: [(Text, TypedProgram)]
validIndependentLowererPrograms =
  [ (name, programValue)
  | (name, programValue, _) <- scalarBindingExpectedLoweredPrograms
  ]
    <> [ (name, programValue)
       | (name, programValue, _) <- directRecursionExpectedLoweredPrograms
       ]
    <> [ (name, programValue)
       | (name, programValue, _) <- closureRecursionExpectedLoweredPrograms
       ]
    <> lowererBoundaryPrograms
    <> lowererStructuralBoundaryPrograms
    <> scalarPatternCaseLowererBoundaryPrograms

invalidLowererBoundaryPrograms :: [(Text, TypedProgram)]
invalidLowererBoundaryPrograms =
  [ ("closure-shape-flattened-recipe", closureShapeFlattenedRecipeLowererProgram),
    ("direct-shape-staged-recipe", directShapeStagedRecipeLowererProgram),
    ("callable-shape-body-disagreement", callableShapeBodyDisagreementLowererProgram),
    ("variable-binder-reference-mismatch", variableBinderReferenceMismatchLowererProgram),
    ("direct-flattened-representation", directFlattenedRepresentationLowererProgram),
    ("direct-shaped-closure-value-self-recursion", directShapedClosureValueSelfRecursiveLowererProgram),
    ("shape-rejected-self-recursion", shapeRejectedSelfRecursiveLowererProgram),
    ("shape-rejected-mutual-recursion", shapeRejectedMutualRecursiveLowererProgram),
    ("shape-rejected-binder-shadow-control", shapeRejectedBinderShadowControlLowererProgram),
    ("bare-function-value", bareFunctionLowererProgram),
    ("partial-direct-call", partialCallLowererProgram)
  ]

independentLowererPrograms :: [(Text, TypedProgram)]
independentLowererPrograms =
  validIndependentLowererPrograms <> invalidLowererBoundaryPrograms

lowererStructuralBoundaryPrograms :: [(Text, TypedProgram)]
lowererStructuralBoundaryPrograms =
  [ ("managed-scalar-entry", managedScalarLowererProgram),
    ("conditional-entry", conditionalLowererProgram),
    ("managed-pattern-scrutinee", managedPatternScrutineeLowererProgram)
  ]

reviewLowererBoundaryPrograms :: [(Text, TypedProgram)]
reviewLowererBoundaryPrograms =
  [ ("lifted-lambda-failure-preorder", liftedLambdaFailurePreorderProgram),
    ("exported-scalar-lifted-lambda-name-collision", exportedScalarLiftedLambdaNameCollisionProgram)
  ]

liftedLambdaFailurePreorderProgram :: TypedProgram
liftedLambdaFailurePreorderProgram =
  expectedScalarProgram
    intInfo
    ( TypedApplyExpr
        intInfo
        ( TypedLambdaExpr
            lambdaInfo
            parameterBinder
            parameterName
            (TypedIfExpr intInfo (boolExpr True) (intExpr 1) (intExpr 2))
        )
        (TypedIfExpr intInfo (boolExpr True) (intExpr 3) (intExpr 4))
    )
  where
    parameterName = resolvedName "item"
    parameterBinder = TypedBinderId (modulePath, [0, 0], parameterName)
    lambdaInfo = stagedFunctionInfo [("item", intInfo)] intInfo

exportedScalarLiftedLambdaNameCollisionProgram :: TypedProgram
exportedScalarLiftedLambdaNameCollisionProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        [TypedModuleExport TypedValueNamespace "item"]
        (TypedModuleInterface [TypedValueInterface itemName itemScheme] [] [] [])
        []
        [ TypedLetStatement itemBinder itemName (TypedSpan 1 1) itemScheme (intExpr 1),
          TypedExpressionStatement
            (TypedSpan 2 1)
            (TypedLambdaExpr lambdaInfo parameterBinder itemName (boundVariableExpr itemName intInfo parameterBinder))
        ]
        lambdaInfo
    ]
    modulePath
  where
    itemName = resolvedName "item"
    itemBinder = TypedBinderId (modulePath, [0], itemName)
    itemScheme = scalarScheme itemBinder intInfo
    parameterBinder = TypedBinderId (modulePath, [1], itemName)
    lambdaInfo = stagedFunctionInfo [("item", intInfo)] intInfo

managedScalarLowererProgram :: TypedProgram
managedScalarLowererProgram =
  expectedScalarProgram
    textInfo
    (TypedLiteralExpr textInfo (TypedTextLiteral "managed"))

managedPatternScrutineeLowererProgram :: TypedProgram
managedPatternScrutineeLowererProgram =
  expectedScalarProgram
    textInfo
    ( TypedPatternCaseExpr
        textInfo
        (textExpr "managed")
        [ TypedCaseArm
            (TypedWildcardPattern textInfo)
            Nothing
            (textExpr "managed")
        ]
    )

conditionalLowererProgram :: TypedProgram
conditionalLowererProgram =
  expectedScalarProgram
    intInfo
    (TypedIfExpr intInfo (boolExpr True) (intExpr 1) (intExpr 2))

closureShapeLowererProgram :: TypedProgram
closureShapeLowererProgram =
  expectedFunctionProgram
    []
    [boolIdentityFunction]
    (variableExpr "identity" boolCallableInfo)

closureShapeApplicationLowererProgram :: TypedProgram
closureShapeApplicationLowererProgram =
  expectedFunctionProgram
    []
    [boolIdentityFunction]
    ( directCall
        "identity"
        [boolInfo]
        boolInfo
        [binaryExpr boolInfo "==" (boolExpr True) (boolExpr False)]
    )

callableParameterShadowsTopLevelLowererProgram :: TypedProgram
callableParameterShadowsTopLevelLowererProgram =
  expectedFunctionProgram
    []
    [boolCombineFunction, applyCombineParameterFunction]
    (boolExpr True)

callableParameterValueShadowsEnclosingFunctionLowererProgram :: TypedProgram
callableParameterValueShadowsEnclosingFunctionLowererProgram =
  expectedFunctionProgram
    []
    [applyFunction, shadowingForwardFunction]
    (boolExpr True)

closureValuedParameterLowererProgram :: TypedProgram
closureValuedParameterLowererProgram =
  expectedFunctionProgram
    []
    [applyFunction]
    (boolExpr True)

closureValuedResultLowererProgram :: TypedProgram
closureValuedResultLowererProgram =
  expectedFunctionProgram
    []
    [boolIdentityFunction, chooseFunction]
    (boolExpr True)

directFlattenedRepresentationLowererProgram :: TypedProgram
directFlattenedRepresentationLowererProgram =
  expectedFunctionProgram
    []
    [boolCombineFunction]
    (variableExpr "combine" (functionInfo [("left", boolInfo), ("right", boolInfo)] boolInfo))

nonConcreteClosureRepresentationLowererProgram :: TypedProgram
nonConcreteClosureRepresentationLowererProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        [ TypedSignatureStatement signatureBinder functionName (TypedSpan 1 1) (polymorphicScheme signatureBinder),
          TypedLetStatement
            bindingBinder
            functionName
            (TypedSpan 2 1)
            (polymorphicScheme bindingBinder)
            ( TypedLambdaExpr
                polymorphicInfo
                parameterBinder
                parameterName
                (TypedVariableExpr parameterInfo parameterName (Just parameterBinder))
            ),
          TypedExpressionStatement (TypedSpan 3 1) (boolExpr True)
        ]
        boolInfo
    ]
    modulePath
  where
    typeParameter = TypedTypeParameterId 0
    parameterName = resolvedName "item"
    functionName = resolvedName "identity"
    signatureBinder = TypedBinderId (modulePath, [0], functionName)
    bindingBinder = TypedBinderId (modulePath, [1], functionName)
    parameterBinder = TypedBinderId (modulePath, [1, 0], parameterName)
    parameterInfo =
      TypedNodeInfo
        (TypedTypeParameterType typeParameter)
        (TypedRepresentationParameterRecipe typeParameter)
        []
        []
    polymorphicInfo =
      TypedNodeInfo
        (TypedFunctionType (typedExpressionType parameterInfo) (typedExpressionType parameterInfo))
        ( TypedClosureRecipe
            [typedExpressionRecipe parameterInfo]
            (typedExpressionRecipe parameterInfo)
        )
        []
        []
    polymorphicScheme owner =
      TypedScheme
        owner
        [typeParameter]
        []
        []
        (typedExpressionType polymorphicInfo)
        (typedExpressionRecipe polymorphicInfo)
        (Just TypedClosureCallableShape)

callableShapeBodyDisagreementLowererProgram :: TypedProgram
callableShapeBodyDisagreementLowererProgram =
  rewriteChooserShape
    ( expectedFunctionProgram
        []
        [ boolCombineFunction,
          ExpectedFunction
            "choose"
            [("ignored", boolInfo)]
            binaryCallableInfo
            TypedClosureCallableShape
            (variableExpr "combine" binaryCallableInfo)
        ]
        (boolExpr True)
    )
  where
    binaryCallableInfo = functionInfo [("left", boolInfo), ("right", boolInfo)] boolInfo
    stagedChooserInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (typedExpressionType binaryCallableInfo))
        ( TypedClosureRecipe
            [TypedBoolRecipe]
            (TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe))
        )
        []
        []
    rewriteChooserShape programValue =
      case programValue of
        TypedProgram prelude [TypedModule path source imports exports interface recursiveGroups statements moduleInfo] entryPath ->
          TypedProgram
            prelude
            [ TypedModule
                path
                source
                imports
                exports
                interface
                recursiveGroups
                (map rewriteStatement statements)
                moduleInfo
            ]
            entryPath
        _ -> error "callable shape/body disagreement lowerer fixture changed shape"
    chooserName = resolvedName "choose"
    rewriteStatement statement =
      case statement of
        TypedSignatureStatement owner name spanValue schemeValue
          | name == chooserName ->
              TypedSignatureStatement owner name spanValue (rewriteScheme schemeValue)
        TypedLetStatement owner name spanValue schemeValue (TypedLambdaExpr _ parameterOwner parameterName body)
          | name == chooserName ->
              TypedLetStatement
                owner
                name
                spanValue
                (rewriteScheme schemeValue)
                (TypedLambdaExpr stagedChooserInfo parameterOwner parameterName body)
        _ -> statement
    rewriteScheme (TypedScheme owner parameters evidence primitive typeValue _ shape) =
      TypedScheme owner parameters evidence primitive typeValue (typedExpressionRecipe stagedChooserInfo) shape

closureShapeFlattenedRecipeLowererProgram :: TypedProgram
closureShapeFlattenedRecipeLowererProgram =
  rewriteRootRecipe
    ( expectedFunctionProgram
        []
        [boolCombineFunction {expectedFunctionShape = TypedClosureCallableShape}]
        (boolExpr True)
    )
  where
    flattenedInfo = functionInfo [("left", boolInfo), ("right", boolInfo)] boolInfo
    rewriteRootRecipe programValue =
      case programValue of
        TypedProgram prelude [TypedModule path source imports exports interface recursiveGroups statements moduleInfo] entryPath ->
          TypedProgram
            prelude
            [TypedModule path source imports exports interface recursiveGroups (map rewriteStatement statements) moduleInfo]
            entryPath
        _ -> error "closure flattened-recipe lowerer fixture changed shape"
    rewriteStatement statement =
      case statement of
        TypedSignatureStatement owner name spanValue schemeValue ->
          TypedSignatureStatement owner name spanValue (rewriteScheme schemeValue)
        TypedLetStatement owner name spanValue schemeValue (TypedLambdaExpr _ parameterOwner parameterName body) ->
          TypedLetStatement
            owner
            name
            spanValue
            (rewriteScheme schemeValue)
            (TypedLambdaExpr flattenedInfo parameterOwner parameterName body)
        other -> other
    rewriteScheme (TypedScheme owner parameters evidence primitive typeValue _ shape) =
      TypedScheme owner parameters evidence primitive typeValue (typedExpressionRecipe flattenedInfo) shape

directShapeStagedRecipeLowererProgram :: TypedProgram
directShapeStagedRecipeLowererProgram =
  rewriteRootRecipe
    (expectedFunctionProgram [] [boolCombineFunction] (boolExpr True))
  where
    stagedInfo =
      TypedNodeInfo
        (TypedFunctionType TypedBoolType (TypedFunctionType TypedBoolType TypedBoolType))
        (TypedClosureRecipe [TypedBoolRecipe] (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe))
        []
        []
    rewriteRootRecipe programValue =
      case programValue of
        TypedProgram prelude [TypedModule path source imports exports interface recursiveGroups statements moduleInfo] entryPath ->
          TypedProgram
            prelude
            [TypedModule path source imports exports interface recursiveGroups (map rewriteStatement statements) moduleInfo]
            entryPath
        _ -> error "direct staged-recipe lowerer fixture changed shape"
    rewriteStatement statement =
      case statement of
        TypedSignatureStatement owner name spanValue schemeValue ->
          TypedSignatureStatement owner name spanValue (rewriteScheme schemeValue)
        TypedLetStatement owner name spanValue schemeValue (TypedLambdaExpr _ parameterOwner parameterName body) ->
          TypedLetStatement
            owner
            name
            spanValue
            (rewriteScheme schemeValue)
            (TypedLambdaExpr stagedInfo parameterOwner parameterName body)
        other -> other
    rewriteScheme (TypedScheme owner parameters evidence primitive typeValue _ shape) =
      TypedScheme owner parameters evidence primitive typeValue (typedExpressionRecipe stagedInfo) shape

variableBinderReferenceMismatchLowererProgram :: TypedProgram
variableBinderReferenceMismatchLowererProgram =
  case expectedFunctionProgram [] [boolIdentityFunction] (variableExpr "identity" boolCallableInfo) of
    TypedProgram prelude [TypedModule path source imports exports interface recursiveGroups statements moduleInfo] entryPath ->
      TypedProgram
        prelude
        [TypedModule path source imports exports interface recursiveGroups (map corruptTerminal statements) moduleInfo]
        entryPath
    _ -> error "variable binder-reference lowerer fixture changed shape"
  where
    wrongBinder = TypedBinderId (modulePath, [999], resolvedName "identity")
    corruptTerminal statement =
      case statement of
        TypedExpressionStatement spanValue (TypedVariableExpr info name _) ->
          TypedExpressionStatement spanValue (TypedVariableExpr info name (Just wrongBinder))
        other -> other

duplicateParameterLowererProgram :: TypedProgram
duplicateParameterLowererProgram =
  expectedFunctionProgram
    []
    [ ExpectedFunction
        "chooseSecond"
        [("item", intInfo), ("item", intInfo)]
        intInfo
        TypedDirectCallableShape
        (variableExpr "item" intInfo)
    ]
    (directCall "chooseSecond" [intInfo, intInfo] intInfo [intExpr 1, intExpr 2])

selfRecursiveDuplicateParameterLowererProgram :: TypedProgram
selfRecursiveDuplicateParameterLowererProgram =
  expectedFunctionProgramWithRecursiveGroups
    [["loop"]]
    []
    [ ExpectedFunction
        "loop"
        [("item", intInfo), ("item", intInfo)]
        intInfo
        TypedDirectCallableShape
        ( directCall
            "loop"
            [intInfo, intInfo]
            intInfo
            [variableExpr "item" intInfo, variableExpr "item" intInfo]
        )
    ]
    (directCall "loop" [intInfo, intInfo] intInfo [intExpr 1, intExpr 2])

duplicateFunctionLowererProgram :: TypedProgram
duplicateFunctionLowererProgram =
  expectedFunctionProgram
    []
    [ ExpectedFunction
        "identity"
        [("first", intInfo)]
        intInfo
        TypedDirectCallableShape
        (variableExpr "first" intInfo),
      ExpectedFunction
        "identity"
        [("second", intInfo)]
        intInfo
        TypedDirectCallableShape
        (variableExpr "second" intInfo)
    ]
    (directCall "identity" [intInfo] intInfo [intExpr 1])

invalidScalarBindingRhsProgram :: TypedProgram
invalidScalarBindingRhsProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        [ TypedLetStatement
            seedBinder
            seedName
            (TypedSpan 1 1)
            seedScheme
            (TypedIfExpr intInfo (boolExpr True) (intExpr 1) (intExpr 2)),
          TypedExpressionStatement (TypedSpan 2 1) (intExpr 1)
        ]
        intInfo
    ]
    modulePath
  where
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [0], seedName)
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64) Nothing

recursionDescendantFailureOrderLowererProgram :: TypedProgram
recursionDescendantFailureOrderLowererProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        [TypedRecursiveGroup [loopBinder]]
        ( scalarStatement
            <> map
              (bindExpectedStatementVariables bindings)
              (expectedFunctionStatements 1 2 loopFunction)
            <> [ TypedExpressionStatement
                   (TypedSpan 4 1)
                   (bindExpectedExpressionVariables bindings (directCall "loop" [intInfo] intInfo [intExpr 1]))
               ]
        )
        intInfo
    ]
    modulePath
  where
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [0], seedName)
    loopName = resolvedName "loop"
    loopBinder = TypedBinderId (modulePath, [2], loopName)
    bindings = Map.fromList [(seedName, seedBinder), (loopName, loopBinder)]
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64) Nothing
    scalarStatement =
      [TypedLetStatement seedBinder seedName (TypedSpan 1 1) seedScheme (intExpr 1)]
    loopFunction =
      ExpectedFunction
        "loop"
        [("item", intInfo)]
        intInfo
        TypedDirectCallableShape
        ( binaryExpr
            intInfo
            "+"
            (directCall "loop" [intInfo] intInfo [variableExpr "item" intInfo])
            (variableExpr "seed" intInfo)
        )

interleavedCaptureMutualRecursiveLowererProgram :: TypedProgram
interleavedCaptureMutualRecursiveLowererProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        [TypedRecursiveGroup [leftBinder, rightBinder]]
        ( map
            (bindExpectedStatementVariables bindings)
            (expectedFunctionStatements 0 1 leftFunction)
            <> [TypedLetStatement seedBinder seedName (TypedSpan 3 1) seedScheme (intExpr 1)]
            <> map
              (bindExpectedStatementVariables bindings)
              (expectedFunctionStatements 3 4 rightFunction)
            <> [ TypedExpressionStatement
                   (TypedSpan 6 1)
                   (bindExpectedExpressionVariables bindings (directCall "left" [intInfo] intInfo [intExpr 1]))
               ]
        )
        intInfo
    ]
    modulePath
  where
    leftName = resolvedName "left"
    leftBinder = TypedBinderId (modulePath, [1], leftName)
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [2], seedName)
    rightName = resolvedName "right"
    rightBinder = TypedBinderId (modulePath, [4], rightName)
    bindings =
      Map.fromList
        [ (leftName, leftBinder),
          (seedName, seedBinder),
          (rightName, rightBinder)
        ]
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64) Nothing
    leftFunction =
      ExpectedFunction
        "left"
        [("item", intInfo)]
        intInfo
        TypedClosureCallableShape
        (directCall "right" [intInfo] intInfo [variableExpr "item" intInfo])
    rightFunction =
      ExpectedFunction
        "right"
        [("item", intInfo)]
        intInfo
        TypedClosureCallableShape
        ( directCall
            "left"
            [intInfo]
            intInfo
            [binaryExpr intInfo "+" (variableExpr "item" intInfo) (variableExpr "seed" intInfo)]
        )

capturingLowererProgram :: TypedProgram
capturingLowererProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        ( scalarStatement
            <> map
              (bindExpectedStatementVariables bindings)
              (expectedFunctionStatements 1 2 addSeedFunction)
            <> [ TypedExpressionStatement
                   (TypedSpan 4 1)
                   (bindExpectedExpressionVariables bindings (directCall "addSeed" [intInfo] intInfo [intExpr 41]))
               ]
        )
        intInfo
    ]
    modulePath
  where
    seedName = resolvedName "seed"
    seedBinder = TypedBinderId (modulePath, [0], seedName)
    addSeedName = resolvedName "addSeed"
    addSeedBinder = TypedBinderId (modulePath, [2], addSeedName)
    bindings = Map.fromList [(seedName, seedBinder), (addSeedName, addSeedBinder)]
    seedScheme = TypedScheme seedBinder [] [] [] TypedIntType (TypedSignedIntegerRecipe 64) Nothing
    scalarStatement =
      [TypedLetStatement seedBinder seedName (TypedSpan 1 1) seedScheme (intExpr 1)]
    addSeedFunction =
      ExpectedFunction
        "addSeed"
        [("item", intInfo)]
        intInfo
        TypedDirectCallableShape
        (binaryExpr intInfo "+" (variableExpr "item" intInfo) (variableExpr "seed" intInfo))

closureShapedSelfRecursiveLowererProgram :: TypedProgram
closureShapedSelfRecursiveLowererProgram =
  expectedFunctionProgramWithRecursiveGroups
    [["loop"]]
    []
    [ ExpectedFunction
        "loop"
        [("item", intInfo)]
        intInfo
        TypedClosureCallableShape
        (directCall "loop" [intInfo] intInfo [variableExpr "item" intInfo])
    ]
    (boolExpr True)

directShapedClosureValueSelfRecursiveLowererProgram :: TypedProgram
directShapedClosureValueSelfRecursiveLowererProgram =
  expectedFunctionProgramWithRecursiveGroups
    [["loop"]]
    []
    [ applyFunction,
      closurePassingLoopFunction {expectedFunctionShape = TypedDirectCallableShape}
    ]
    (boolExpr True)

nestedLambdaClosureValueSelfRecursiveLowererProgram :: TypedProgram
nestedLambdaClosureValueSelfRecursiveLowererProgram =
  expectedFunctionProgramWithRecursiveGroups
    [["loop"]]
    []
    [applyFunction, nestedLambdaClosurePassingLoopFunction]
    (boolExpr True)

shapeRejectedSelfRecursiveLowererProgram :: TypedProgram
shapeRejectedSelfRecursiveLowererProgram =
  shapeRejectedCycleLowererProgram [("loop", "loop")]

shapeRejectedMutualRecursiveLowererProgram :: TypedProgram
shapeRejectedMutualRecursiveLowererProgram =
  shapeRejectedCycleLowererProgram [("left", "right"), ("right", "left")]

shapeRejectedCycleLowererProgram :: [(Text, Text)] -> TypedProgram
shapeRejectedCycleLowererProgram functions =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        [ TypedRecursiveGroup
            [ binders Map.! resolvedName name
            | (name, _) <- functions
            ]
        ]
        (concatMap functionStatements indexedFunctions <> [TypedExpressionStatement (TypedSpan (length functions * 2 + 1) 1) (boolExpr True)])
        boolInfo
    ]
    modulePath
  where
    indexedFunctions = zip [0 ..] functions
    binders =
      Map.fromList
        [ (resolvedName name, TypedBinderId (modulePath, [bindingIndex], resolvedName name))
        | (functionIndex, (name, _)) <- indexedFunctions,
          let bindingIndex = functionIndex * 2 + 1
        ]
    functionStatements (functionIndex, (name, target)) =
      let signatureIndex = functionIndex * 2
          bindingIndex = signatureIndex + 1
          function = ExpectedFunction name [("item", boolInfo)] boolInfo TypedDirectCallableShape (boolExpr True)
          functionName = resolvedName name
          signatureBinder = TypedBinderId (modulePath, [signatureIndex], functionName)
          bindingBinder = TypedBinderId (modulePath, [bindingIndex], functionName)
       in [ TypedSignatureStatement
              signatureBinder
              functionName
              (TypedSpan (signatureIndex + 1) 1)
              (functionScheme signatureIndex function),
            bindExpectedStatementVariables
              binders
              ( TypedLetStatement
                  bindingBinder
                  functionName
                  (TypedSpan (bindingIndex + 1) 1)
                  (functionScheme bindingIndex function)
                  (shapeRejectedConditionalBody bindingIndex target)
              )
          ]
    shapeRejectedConditionalBody statementIndex target =
      TypedIfExpr
        boolCallableInfo
        (boolExpr True)
        (branchLambda statementIndex 1 target)
        (branchLambda statementIndex 2 target)
    branchLambda statementIndex branchIndex target =
      let parameterName = resolvedName "item"
          parameterBinder = TypedBinderId (modulePath, [statementIndex, 0, branchIndex], parameterName)
       in TypedLambdaExpr
            boolCallableInfo
            parameterBinder
            parameterName
            (directCall target [boolInfo] boolInfo [TypedVariableExpr boolInfo parameterName (Just parameterBinder)])

shapeRejectedBinderShadowControlLowererProgram :: TypedProgram
shapeRejectedBinderShadowControlLowererProgram =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        [ TypedSignatureStatement signatureBinder functionName (TypedSpan 1 1) (functionScheme 0 function),
          TypedLetStatement
            bindingBinder
            functionName
            (TypedSpan 2 1)
            (functionScheme 1 function)
            ( TypedIfExpr
                functionNodeInfo
                (boolExpr True)
                (branchLambda 1)
                (branchLambda 2)
            ),
          TypedExpressionStatement (TypedSpan 3 1) (boolExpr True)
        ]
        boolInfo
    ]
    modulePath
  where
    functionName = resolvedName "loop"
    function = ExpectedFunction "loop" [("loop", boolCallableInfo)] boolInfo TypedDirectCallableShape (boolExpr True)
    signatureBinder = TypedBinderId (modulePath, [0], functionName)
    bindingBinder = TypedBinderId (modulePath, [1], functionName)
    functionNodeInfo = functionInfo [("loop", boolCallableInfo)] boolInfo
    branchLambda branchIndex =
      let parameterBinder = TypedBinderId (modulePath, [1, 0, branchIndex], functionName)
       in TypedLambdaExpr
            functionNodeInfo
            parameterBinder
            functionName
            (TypedApplyExpr boolInfo (TypedVariableExpr boolCallableInfo functionName (Just parameterBinder)) (boolExpr True))

bareFunctionLowererProgram :: TypedProgram
bareFunctionLowererProgram =
  expectedFunctionProgram
    []
    [identityFunction]
    (TypedVariableExpr (functionInfo [("item", intInfo)] intInfo) (resolvedName "identity") Nothing)

partialCallLowererProgram :: TypedProgram
partialCallLowererProgram =
  expectedFunctionProgram
    []
    [combineFunction]
    ( TypedApplyExpr
        (functionInfo [("right", intInfo)] intInfo)
        (TypedVariableExpr (functionInfo [("left", intInfo), ("right", intInfo)] intInfo) (resolvedName "combine") Nothing)
        (intExpr 1)
    )

importedDirectCallLowererProgram :: TypedProgram
importedDirectCallLowererProgram =
  TypedProgram Nothing [providerModule, entry] modulePath
  where
    providerPath = ["Library", "Functions"]
    providerName = TypedResolvedName TypedCurrentModule TypedValueNamespace "foreign"
    importedName = TypedResolvedName (TypedImportedModule providerPath) TypedValueNamespace "foreign"
    providerOwner = TypedBinderId (providerPath, [0], providerName)
    providerParameterName = TypedResolvedName TypedCurrentModule TypedValueNamespace "item"
    providerParameterBinder = TypedBinderId (providerPath, [0, 0], providerParameterName)
    providerInfo = functionInfo [("item", intInfo)] intInfo
    providerScheme =
      TypedScheme
        providerOwner
        []
        []
        []
        (TypedFunctionType TypedIntType TypedIntType)
        (TypedClosureRecipe [TypedSignedIntegerRecipe 64] (TypedSignedIntegerRecipe 64))
        (Just TypedDirectCallableShape)
    providerModule =
      TypedModule
        providerPath
        (TypedSourcePath "src/Library/Functions.jz")
        []
        [TypedModuleExport TypedValueNamespace "foreign"]
        (TypedModuleInterface [TypedValueInterface providerName providerScheme] [] [] [])
        []
        [ TypedLetStatement
            providerOwner
            providerName
            (TypedSpan 1 1)
            providerScheme
            ( TypedLambdaExpr
                providerInfo
                providerParameterBinder
                providerParameterName
                (TypedVariableExpr intInfo providerParameterName (Just providerParameterBinder))
            )
        ]
        unitInfo
    callExpression =
      TypedApplyExpr
        intInfo
        (TypedVariableExpr providerInfo importedName (Just providerOwner))
        (intExpr 1)
    entry =
      TypedModule
        modulePath
        validSourcePath
        [TypedResolvedImport (TypedSpan 1 1) providerPath Nothing (Just ["foreign"])]
        []
        (TypedModuleInterface [] [] [] [])
        []
        [TypedExpressionStatement (TypedSpan 1 1) callExpression]
        intInfo

producerEdgeFixtures :: [(Text, Fixture)]
producerEdgeFixtures =
  scalarBindingProducerFixtures
    <> managedTextProducerFixtures
    <> managedTextOperationProducerFixtures
    <> managedTextKernelBoundaryFixtures
    <> managedTextExclusionFixtures
    <> [ ( "scalar-pattern-case",
           sourceFixtureNoExports
             "scalar-pattern-case"
             "case True { | True -> 1 | _ -> 2 }."
         ),
         ( "scalar-pattern-case-variable-guards",
           sourceFixtureNoExports
             "scalar-pattern-case-variable-guards"
             "case 2 { | item if item > 2 -> item | fallback -> fallback + 1 }."
         ),
         ( "scalar-pattern-case-repeated-literal-guards",
           sourceFixtureNoExports
             "scalar-pattern-case-repeated-literal-guards"
             "case 2 { | 2 if False -> 10 | 2 if True -> 20 | _ -> 30 }."
         ),
         ( "scalar-pattern-case-capture",
           sourceFixtureNoExports
             "scalar-pattern-case-capture"
             ( Text.unlines
                 [ "seed = 40.",
                   "choose = \\(item) -> case item { | current if current > 0 -> current + seed | _ -> seed }.",
                   "choose 2."
                 ]
             )
         ),
         ( "scalar-pattern-case-closure-result",
           sourceFixtureNoExports
             "scalar-pattern-case-closure-result"
             ( Text.unlines
                 [ "choose = \\(flag) -> case flag { | True -> \\(item) -> item | _ -> \\(item) -> 0 }.",
                   "(choose True) 7."
                 ]
             )
         ),
         ( "scalar-pattern-case-tail-function",
           sourceFixtureNoExports
             "scalar-pattern-case-tail-function"
             ( Text.unlines
                 [ "loop :: Int -> Int.",
                   "loop = \\(item) -> case item { | 0 -> 0 | 1 -> loop 0 | next if next == 2 -> next | _ -> 3 }.",
                   "loop 1."
                 ]
             )
         ),
         ( "nested-tail-if-alternatives",
           sourceFixtureNoExports
             "nested-tail-if-alternatives"
             ( Text.unlines
                 [ "chooseNestedIf :: Bool -> Bool -> Int -> Int.",
                   "chooseNestedIf = \\(outer, inner, item) -> if outer then (if inner then item else 1) else (case item { | 0 -> 2 | _ -> 3 }).",
                   "chooseNestedIf True False 9."
                 ]
             )
         ),
         ( "nested-tail-case-bodies",
           sourceFixtureNoExports
             "nested-tail-case-bodies"
             ( Text.unlines
                 [ "chooseNestedCase :: Int -> Bool -> Int.",
                   "chooseNestedCase = \\(item, flag) -> case item { | 0 -> if flag then 1 else 2 | _ -> case item { | 1 -> 3 | _ -> 4 } }.",
                   "chooseNestedCase 0 True."
                 ]
             )
         ),
         ( "pattern-case-in-conditional-branch",
           sourceFixtureNoExports
             "pattern-case-in-conditional-branch"
             "if True then case 1 { | 1 -> 10 | _ -> 20 } else 30."
         ),
         ( "conditional-in-pattern-case-guard",
           sourceFixtureNoExports
             "conditional-in-pattern-case-guard"
             "case 1 { | 1 if if True then False else True -> 10 | _ -> 20 }."
         ),
         ( "pattern-case-in-pattern-case-body",
           sourceFixtureNoExports
             "pattern-case-in-pattern-case-body"
             "case True { | True -> case 1 { | 1 -> 10 | _ -> 20 } | _ -> 30 }."
         ),
         ( "pattern-case-scrutinee-pattern-case",
           sourceFixtureNoExports
             "pattern-case-scrutinee-pattern-case"
             "case (case True { | True -> 1 | _ -> 2 }) { | 1 -> 10 | _ -> 20 }."
         ),
         ( "pattern-case-ambient-scalar",
           sourceFixtureNoExports
             "pattern-case-ambient-scalar"
             ( Text.unlines
                 [ "identity :: Int -> Int.",
                   "identity = \\(item) -> item.",
                   "seed = identity 1.",
                   "chosen = case seed { | item if item == seed -> item | _ -> 0 }.",
                   "chosen + seed."
                 ]
             )
         ),
         ( "pattern-case-captured-scalar",
           sourceFixtureNoExports
             "pattern-case-captured-scalar"
             ( Text.unlines
                 [ "seed = 1.",
                   "choose = \\(item) -> case item { | current if current == seed -> current | _ -> seed }.",
                   "choose seed."
                 ]
             )
         ),
         ( "pattern-case-call-argument",
           sourceFixtureNoExports
             "pattern-case-call-argument"
             "(\\(item) -> item) (case 1 { | 1 -> 2 | _ -> 3 })."
         ),
         ( "pattern-case-final-guarded-catch-all",
           sourceFixtureNoExports
             "pattern-case-final-guarded-catch-all"
             "case True { | _ if True -> 1 }."
         ),
         ( "pattern-case-missing-final-catch-all",
           sourceFixtureNoExports
             "pattern-case-missing-final-catch-all"
             "case True { | True -> 1 }."
         ),
         ( "pattern-case-unguarded-non-final-wildcard",
           sourceFixtureNoExports
             "pattern-case-unguarded-non-final-wildcard"
             "case True { | _ -> 1 | _ -> 2 }."
         ),
         ( "pattern-case-unguarded-non-final-variable",
           sourceFixtureNoExports
             "pattern-case-unguarded-non-final-variable"
             "case True { | item -> 1 | _ -> 2 }."
         ),
         ( "pattern-case-managed-scrutinee",
           sourceFixtureNoExports
             "pattern-case-managed-scrutinee"
             "case \"managed\" { | _ -> 1 }."
         ),
         ( "pattern-case-constructor-pattern",
           sourceFixtureNoExports
             "pattern-case-constructor-pattern"
             "data Maybe = Nothing. case Nothing { | Nothing -> 1 }."
         ),
         ( "pattern-case-list-pattern",
           sourceFixtureNoExports
             "pattern-case-list-pattern"
             "case [1] { | [1] -> 1 | _ -> 2 }."
         ),
         ( "pattern-case-tuple-pattern",
           sourceFixtureNoExports
             "pattern-case-tuple-pattern"
             "case (True, False) { | (True, False) -> 1 | _ -> 2 }."
         ),
         ( "pattern-case-as-pattern",
           sourceFixtureNoExports
             "pattern-case-as-pattern"
             "case True { | whole @ True -> 1 | _ -> 2 }."
         ),
         ( "pattern-case-or-pattern",
           sourceFixtureNoExports
             "pattern-case-or-pattern"
             "case True { | True | False -> 1 }."
         ),
         ( "pattern-case-non-bool-guard",
           sourceFixtureNoExports
             "pattern-case-non-bool-guard"
             "case True { | True if 1 -> 1 | _ -> 2 }."
         ),
         ( "pattern-case-incompatible-arm-results",
           sourceFixtureNoExports
             "pattern-case-incompatible-arm-results"
             "case True { | True -> 1 | _ -> False }."
         ),
         ("conditional-function-parameter", sourceFixtureNoExports "conditional-function-parameter" conditionalFunctionParameterSource),
         ("conditional-captured-scalar", sourceFixtureNoExports "conditional-captured-scalar" conditionalCapturedScalarSource),
         ("conditional-tail-call-function", sourceFixtureNoExports "conditional-tail-call-function" conditionalTailCallFunctionSource),
         ("conditional-closure-result-application", sourceFixtureNoExports "conditional-closure-result-application" conditionalClosureResultApplicationSource),
         ("nested-conditionals", sourceFixtureNoExports "nested-conditionals" nestedConditionalsSource),
         ("empty-module", sourceFixtureNoExports "empty-module" ""),
         ( "default-exported-polymorphic-callable",
           sourceFixture
             "default-exported-polymorphic-callable"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "identity :: a -> a.",
                   "identity = \\(item) -> item.",
                   "()."
                 ]
             )
         ),
         ( "self-recursive-function-rebinding",
           sourceFixtureNoExports
             "self-recursive-function-rebinding"
             ( Text.unlines
                 [ "loop :: Int -> Int.",
                   "loop = \\(item) -> loop item.",
                   "loop :: Int -> Int.",
                   "loop = \\(item) -> loop (if True then item else item).",
                   "loop 1."
                 ]
             )
         ),
         ( "later-callable-rebinding-calls-nearest-prior",
           sourceFixtureNoExports
             "later-callable-rebinding-calls-nearest-prior"
             ( Text.unlines
                 [ "identity :: Bool -> Bool.",
                   "identity = \\(item) -> item.",
                   "identity :: Bool -> Bool.",
                   "identity = \\(item) -> identity item.",
                   "identity True."
                 ]
             )
         ),
         ( "intervening-scalar-canonical-ownership",
           sourceFixtureNoExports
             "intervening-scalar-canonical-ownership"
             ( Text.unlines
                 [ "a :: Bool -> Bool.",
                   "a = \\(item) -> b item.",
                   "a = True.",
                   "b :: Bool -> Bool.",
                   "b = \\(item) -> a.",
                   "True."
                 ]
             )
         ),
         ( "multiple-intervening-scalars-canonical-ownership",
           sourceFixtureNoExports
             "multiple-intervening-scalars-canonical-ownership"
             ( Text.unlines
                 [ "a :: Bool -> Bool.",
                   "a = \\(item) -> b item.",
                   "a = True.",
                   "a = False.",
                   "b :: Bool -> Bool.",
                   "b = \\(item) -> a.",
                   "True."
                 ]
             )
         ),
         ( "interleaved-callable-scalar-canonical-ownership",
           sourceFixtureNoExports
             "interleaved-callable-scalar-canonical-ownership"
             ( Text.unlines
                 [ "a :: Bool -> Bool.",
                   "a = \\(item) -> b item.",
                   "a = True.",
                   "a :: Bool -> Bool.",
                   "a = \\(item) -> b item.",
                   "a = False.",
                   "b :: Bool -> Bool.",
                   "b = \\(item) -> a.",
                   "True."
                 ]
             )
         ),
         ( "three-same-name-nearest-prior-mutual-recursion",
           sourceFixtureNoExports
             "three-same-name-nearest-prior-mutual-recursion"
             ( Text.unlines
                 [ "identity :: Bool -> Bool.",
                   "identity = \\(item) -> item.",
                   "identity :: Bool -> Bool.",
                   "identity = \\(item) -> item.",
                   "identity :: Bool -> Bool.",
                   "identity = \\(item) -> peer item.",
                   "peer :: Bool -> Bool.",
                   "peer = \\(item) -> identity item.",
                   "True."
                 ]
             )
         ),
         ( "canonical-self-recursion-no-prior",
           sourceFixtureNoExports
             "canonical-self-recursion-no-prior"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> loop item.",
                   "True."
                 ]
             )
         ),
         ( "canonical-mutual-recursion-peers",
           sourceFixtureNoExports
             "canonical-mutual-recursion-peers"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = \\(item) -> right item.",
                   "right :: Bool -> Bool.",
                   "right = \\(item) -> left item.",
                   "True."
                 ]
             )
         ),
         ( "nested-lambda-direct-recursion",
           sourceFixtureNoExports
             "nested-lambda-direct-recursion"
             ( Text.unlines
                 [ "apply :: (Bool -> Bool) -> Bool.",
                   "apply = \\(function) -> function True.",
                   "loop :: Bool -> Bool.",
                   "loop = \\(item) -> apply (\\(nested) -> loop nested).",
                   "loop False."
                 ]
             )
         ),
         ( "nearest-rebinding-mutual-control",
           sourceFixtureNoExports
             "nearest-rebinding-mutual-control"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = \\(item) -> item.",
                   "right :: Bool -> Bool.",
                   "right = \\(item) -> left item.",
                   "left :: Bool -> Bool.",
                   "left = \\(item) -> right item.",
                   "True."
                 ]
             )
         ),
         ( "rebinding-parameter-shadow-control",
           sourceFixtureNoExports
             "rebinding-parameter-shadow-control"
             ( Text.unlines
                 [ "apply :: (Bool -> Bool) -> Bool.",
                   "apply = \\(function) -> function True.",
                   "apply :: (Bool -> Bool) -> Bool.",
                   "apply = \\(apply) -> apply True.",
                   "True."
                 ]
             )
         ),
         ( "rebinding-local-shadow-control",
           sourceFixtureNoExports
             "rebinding-local-shadow-control"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> item.",
                   "loop :: Bool -> Bool.",
                   "loop = \\(item) -> { loop = \\(nested) -> nested. loop item. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-self-alias-recursion",
           sourceFixtureNoExports
             "rejected-self-alias-recursion"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = loop.",
                   "True."
                 ]
             )
         ),
         ( "rejected-mutual-alias-recursion",
           sourceFixtureNoExports
             "rejected-mutual-alias-recursion"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = right.",
                   "right :: Bool -> Bool.",
                   "right = left.",
                   "True."
                 ]
             )
         ),
         ( "rejected-alias-conditional-mutual-recursion",
           sourceFixtureNoExports
             "rejected-alias-conditional-mutual-recursion"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = right.",
                   "right :: Bool -> Bool.",
                   "right = if True then left else left.",
                   "True."
                 ]
             )
         ),
         ( "rejected-operator-alias-self-recursion",
           sourceFixtureNoExports
             "rejected-operator-alias-self-recursion"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "(%%) :: Int -> Int -> Int.",
                   "(%%) = (%%).",
                   "0."
                 ]
             )
         ),
         ( "rejected-eager-operator-conditional-control",
           sourceFixtureNoExports
             "rejected-eager-operator-conditional-control"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "(%%) :: Bool -> Bool -> Bool.",
                   "(%%) = if True %% False then (%%) else (%%).",
                   "True."
                 ]
             )
         ),
         ( "rejected-alias-parameter-shadow-control",
           sourceFixtureNoExports
             "rejected-alias-parameter-shadow-control"
             ( Text.unlines
                 [ "identity :: Bool -> Bool.",
                   "identity = \\(item) -> item.",
                   "loop :: Bool -> Bool.",
                   "loop = (\\(loop) -> loop) identity.",
                   "True."
                 ]
             )
         ),
         ( "rejected-alias-local-shadow-control",
           sourceFixtureNoExports
             "rejected-alias-local-shadow-control"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = {",
                   "  loop :: Bool -> Bool.",
                   "  loop = \\(item) -> item.",
                   "  loop.",
                   "}.",
                   "True."
                 ]
             )
         ),
         ( "rejected-eager-self-before-callable-result-control",
           sourceFixtureNoExports
             "rejected-eager-self-before-callable-result-control"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = { f True. \\(x) -> x. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-nearest-prior-callable-rebinding-recursion",
           sourceFixtureNoExports
             "rejected-block-nearest-prior-callable-rebinding-recursion"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = { inner :: Bool -> Bool. inner = \\(x) -> f x. inner = inner. inner. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-conditional-self-recursion",
           sourceFixtureNoExports
             "rejected-conditional-self-recursion"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> if item then loop False else item.",
                   "loop True."
                 ]
             )
         ),
         ( "rejected-block-conditional-mutual-recursion",
           sourceFixtureNoExports
             "rejected-block-conditional-mutual-recursion"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = \\(item) -> { right item. }.",
                   "right :: Bool -> Bool.",
                   "right = \\(item) -> if item then left False else item.",
                   "left True."
                 ]
             )
         ),
         ( "rejected-block-parameter-shadow-control",
           sourceFixtureNoExports
             "rejected-block-parameter-shadow-control"
             ( Text.unlines
                 [ "apply :: (Bool -> Bool) -> Bool.",
                   "apply = \\(function) -> function True.",
                   "forward :: (Bool -> Bool) -> Bool.",
                   "forward = \\(forward) -> { apply forward. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-later-shadow-control",
           sourceFixtureNoExports
             "rejected-block-later-shadow-control"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> { loop item. loop = \\(nested) -> nested. loop item. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-initializer-self-recursion",
           sourceFixtureNoExports
             "rejected-block-initializer-self-recursion"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> { loop = loop item. item. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-initializer-mutual-recursion",
           sourceFixtureNoExports
             "rejected-block-initializer-mutual-recursion"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = \\(item) -> { right = right item. item. }.",
                   "right :: Bool -> Bool.",
                   "right = \\(item) -> { left = left item. item. }.",
                   "True."
                 ]
             )
         ),
         ( "nested-prior-outer-alias-mutual-recursion",
           sourceFixtureNoExports
             "nested-prior-outer-alias-mutual-recursion"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = \\(item) -> right item.",
                   "right :: Bool -> Bool.",
                   "right = \\(item) -> { left = left. item. }.",
                   "True."
                 ]
             )
         ),
         ( "nested-prior-outer-conditional-alias-mutual-recursion",
           sourceFixtureNoExports
             "nested-prior-outer-conditional-alias-mutual-recursion"
             ( Text.unlines
                 [ "left :: Bool -> Bool.",
                   "left = \\(item) -> right item.",
                   "right :: Bool -> Bool.",
                   "right = \\(item) -> { left = if item then left else left. item. }.",
                   "True."
                 ]
             )
         ),
         ( "nested-self-recursive-lambda-local-ownership",
           sourceFixtureNoExports
             "nested-self-recursive-lambda-local-ownership"
             ( Text.unlines
                 [ "owner :: Bool -> Bool.",
                   "owner = \\(item) -> { loop = \\(nested) -> loop nested. item. }.",
                   "loop :: Bool -> Bool.",
                   "loop = \\(item) -> owner item.",
                   "True."
                 ]
             )
         ),
         ( "accepted-then-rejected-callable-rebinding",
           sourceFixtureNoExports
             "accepted-then-rejected-callable-rebinding"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = \\(item) -> item.",
                   "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> item else \\(item) -> item.",
                   "True."
                 ]
             )
         ),
         ( "rejected-recursive-callable-rebinding-order",
           sourceFixtureNoExports
             "rejected-recursive-callable-rebinding-order"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = \\(item) -> item.",
                   "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> g item else \\(item) -> g item.",
                   "g :: Bool -> Bool.",
                   "g = \\(item) -> f item.",
                   "True."
                 ]
             )
         ),
         ( "rejected-then-accepted-callable-rebinding",
           sourceFixtureNoExports
             "rejected-then-accepted-callable-rebinding"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> item else \\(item) -> item.",
                   "f :: Bool -> Bool.",
                   "f = \\(item) -> item.",
                   "True."
                 ]
             )
         ),
         ( "repeated-rejected-callable-rebinding",
           sourceFixtureNoExports
             "repeated-rejected-callable-rebinding"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> item else \\(item) -> item.",
                   "f :: Bool -> Bool.",
                   "f = if False then \\(item) -> item else \\(item) -> item.",
                   "True."
                 ]
             )
         ),
         ( "scalar-then-rejected-callable-control",
           sourceFixtureNoExports
             "scalar-then-rejected-callable-control"
             ( Text.unlines
                 [ "f = True.",
                   "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> item else \\(item) -> item.",
                   "True."
                 ]
             )
         ),
         ( "accepted-scalar-rejected-callable-rebinding",
           sourceFixtureNoExports
             "accepted-scalar-rejected-callable-rebinding"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = \\(item) -> item.",
                   "f = True.",
                   "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> item else \\(item) -> item.",
                   "True."
                 ]
             )
         ),
         ( "rejected-scalar-accepted-callable-rebinding",
           sourceFixtureNoExports
             "rejected-scalar-accepted-callable-rebinding"
             ( Text.unlines
                 [ "f :: Bool -> Bool.",
                   "f = if True then \\(item) -> item else \\(item) -> item.",
                   "f = True.",
                   "f :: Bool -> Bool.",
                   "f = \\(item) -> item.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-later-signed-shadow-control",
           sourceFixtureNoExports
             "rejected-block-later-signed-shadow-control"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> {",
                   "  observed = loop item.",
                   "  loop :: Bool -> Bool.",
                   "  loop = \\(nested) -> nested.",
                   "  loop item.",
                   "}.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-local-shadow-cycle-control",
           sourceFixtureNoExports
             "rejected-block-local-shadow-cycle-control"
             ( Text.unlines
                 [ "loop :: Bool -> Bool.",
                   "loop = \\(item) -> forward item.",
                   "forward :: Bool -> Bool.",
                   "forward = \\(item) -> { loop = \\(nested) -> nested. loop item. }.",
                   "True."
                 ]
             )
         ),
         ( "rejected-block-parameter-shadow-cycle-control",
           sourceFixtureNoExports
             "rejected-block-parameter-shadow-cycle-control"
             ( Text.unlines
                 [ "forward :: (Bool -> Bool) -> Bool.",
                   "forward = \\(loop) -> { loop True. }.",
                   "identity :: Bool -> Bool.",
                   "identity = \\(item) -> item.",
                   "loop :: Bool -> Bool.",
                   "loop = \\(item) -> forward identity.",
                   "True."
                 ]
             )
         ),
         ( "rejected-operator-value-self-recursion",
           sourceFixtureNoExports
             "rejected-operator-value-self-recursion"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "(%%) :: Int -> Int -> Int.",
                   "(%%) = \\(left, right) -> (%%) left right.",
                   "0."
                 ]
             )
         ),
         ( "rejected-infix-operator-mutual-recursion",
           sourceFixtureNoExports
             "rejected-infix-operator-mutual-recursion"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "operator ~~ tier 2.",
                   "(%%) :: Int -> Int -> Int.",
                   "(%%) = \\(left, right) -> left ~~ right.",
                   "(~~) :: Int -> Int -> Int.",
                   "(~~) = \\(left, right) -> left %% right.",
                   "0."
                 ]
             )
         ),
         ( "rejected-section-operator-mutual-recursion",
           sourceFixtureNoExports
             "rejected-section-operator-mutual-recursion"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "operator ~~ tier 2.",
                   "(%%) :: Int -> Int -> Int.",
                   "(%%) = \\(left, right) -> (left ~~) right.",
                   "(~~) :: Int -> Int -> Int.",
                   "(~~) = \\(left, right) -> (%% right) left.",
                   "0."
                 ]
             )
         ),
         ( "unit-forward-function",
           sourceFixtureNoExports
             "unit-forward-function"
             ( Text.unlines
                 [ "first :: () -> ().",
                   "first = \\(item) -> second item.",
                   "second :: () -> ().",
                   "second = \\(item) -> item.",
                   "first ()."
                 ]
             )
         ),
         ( "curried-first-argument-capture",
           sourceFixtureNoExports
             "curried-first-argument-capture"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "combine :: Int -> Int -> Int.",
                   "combine = \\(left, right) -> left + right.",
                   "use :: Int -> Int.",
                   "use = \\(item) -> combine seed item.",
                   "use 1."
                 ]
             )
         ),
         ( "partial-call-argument-capture",
           sourceFixtureNoExports
             "partial-call-argument-capture"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "combine :: Int -> Int -> Int.",
                   "combine = \\(left, right) -> left + right.",
                   "combine seed."
                 ]
             )
         ),
         ( "partial-call-managed-argument",
           sourceFixtureNoExports
             "partial-call-managed-argument"
             ( Text.unlines
                 [ "keepRight :: Text -> Int -> Int.",
                   "keepRight = \\(ignored, right) -> right.",
                   "keepRight \"managed\"."
                 ]
             )
         ),
         ( "closure-use-argument-failure-order",
           sourceFixtureNoExports
             "closure-use-argument-failure-order"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "apply :: (Int -> Int) -> Int.",
                   "apply = \\(function) -> function seed.",
                   "identity :: Int -> Int.",
                   "identity = \\(item) -> item.",
                   "apply identity.",
                   "[1]."
                 ]
             )
         ),
         ( "non-local-call-argument-capture",
           sourceFixtureNoExports
             "non-local-call-argument-capture"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "__kernel_toFloat64 seed."
                 ]
             )
         ),
         ( "higher-order-parameter",
           sourceFixtureNoExports
             "higher-order-parameter"
             ( Text.unlines
                 [ "ignore :: (Int -> Int) -> Int.",
                   "ignore = \\(function) -> 1.",
                   "1."
                 ]
             )
         ),
         ( "narrow-literal-direct-call",
           sourceFixtureNoExports
             "narrow-literal-direct-call"
             ( Text.unlines
                 [ "narrowIdentity :: Int8 -> Int8.",
                   "narrowIdentity = \\(item) -> item.",
                   "narrowIdentity 1."
                 ]
             )
         ),
         ( "narrow-composite-function-result",
           sourceFixtureNoExports
             "narrow-composite-function-result"
             ( Text.unlines
                 [ "narrowSum :: Bool -> Int8.",
                   "narrowSum = \\(ignored) -> 1 + 2.",
                   "narrowSum True."
                 ]
             )
         ),
         ( "narrow-comparison-operand",
           sourceFixtureNoExports
             "narrow-comparison-operand"
             ( Text.unlines
                 [ "isSmall :: Int8 -> Bool.",
                   "isSmall = \\(item) -> item < 2.",
                   "isSmall 1."
                 ]
             )
         ),
         ( "narrow-root-binary-direct-call",
           sourceFixtureNoExports
             "narrow-root-binary-direct-call"
             ( Text.unlines
                 [ "narrowIdentity :: Int8 -> Int8.",
                   "narrowIdentity = \\(item) -> item.",
                   "narrowIdentity 1 + 2."
                 ]
             )
         ),
         ( "equivalent-scalar-alias-specialization",
           sourceFixtureNoExports
             "equivalent-scalar-alias-specialization"
             ( Text.unlines
                 [ "asInt :: Bool -> Int.",
                   "asInt = \\(ignored) -> 1.",
                   "asInt64 :: Bool -> Int64.",
                   "asInt64 = \\(flag) -> asInt flag.",
                   "acceptInt64 :: Int64 -> Int64.",
                   "acceptInt64 = \\(item) -> item.",
                   "useInt64 :: Bool -> Int64.",
                   "useInt64 = \\(flag) -> acceptInt64 (asInt flag).",
                   "asFloat :: Bool -> Float.",
                   "asFloat = \\(ignored) -> 1.5.",
                   "asFloat64 :: Bool -> Float64.",
                   "asFloat64 = \\(flag) -> asFloat flag.",
                   "acceptFloat64 :: Float64 -> Float64.",
                   "acceptFloat64 = \\(item) -> item.",
                   "acceptFloat64 (asFloat True)."
                 ]
             )
         ),
         ( "earlier-caller-transitive-recursive-capture",
           sourceFixtureNoExports
             "earlier-caller-transitive-recursive-capture"
             ( Text.unlines
                 [ "caller :: Int -> Int.",
                   "caller = \\(item) -> loop item.",
                   "seed = 1.",
                   "loop :: Int -> Int.",
                   "loop = \\(item) -> loop seed.",
                   "caller 1."
                 ]
             )
         ),
         ( "unused-user-defined-operator",
           sourceFixtureNoExports
             "unused-user-defined-operator"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "(%%) :: Int -> Int -> Int.",
                   "(%%) = \\(left, right) -> left + right.",
                   "()."
                 ]
             )
         ),
         ( "root-data-failure-accumulation",
           sourceFixtureNoExports
             "root-data-failure-accumulation"
             ( Text.unlines
                 [ "[1].",
                   "data Box = Box.",
                   "()."
                 ]
             )
         ),
         ( "anonymous-lambda-result",
           sourceFixtureNoExports
             "anonymous-lambda-result"
             "\\(flag) -> flag == True."
         ),
         ( "inline-anonymous-lambda-call",
           sourceFixtureNoExports
             "inline-anonymous-lambda-call"
             "(\\(item) -> item + 1) 41."
         ),
         ( "curried-partial-application",
           sourceFixtureNoExports
             "curried-partial-application"
             ( Text.unlines
                 [ "combine :: Int -> Int -> Int.",
                   "combine = \\(left, right) -> left + right.",
                   "combine 1."
                 ]
             )
         ),
         ( "curried-callable-oversaturation",
           sourceFixtureNoExports
             "curried-callable-oversaturation"
             ( Text.unlines
                 [ "identity :: Int -> Int.",
                   "identity = \\(item) -> item.",
                   "choose :: Bool -> (Int -> Int).",
                   "choose = \\(ignored) -> identity.",
                   "choose False 2."
                 ]
             )
         ),
         ( "curried-partial-higher-order-consumer",
           sourceFixtureNoExports
             "curried-partial-higher-order-consumer"
             ( Text.unlines
                 [ "combine :: Int -> Int -> Int.",
                   "combine = \\(left, right) -> left + right.",
                   "apply :: (Int -> Int) -> Int.",
                   "apply = \\(function) -> function 2.",
                   "apply (combine 1)."
                 ]
             )
         ),
         ( "inline-curried-lambda-call",
           sourceFixtureNoExports
             "inline-curried-lambda-call"
             "(\\(left, right) -> left + right) 20 22."
         ),
         ( "curried-named-function-value",
           sourceFixtureNoExports
             "curried-named-function-value"
             ( Text.unlines
                 [ "combine :: Int -> Int -> Int.",
                   "combine = \\(left, right) -> left + right.",
                   "combine."
                 ]
             )
         ),
         ( "non-callable-oversaturation-diagnostic",
           sourceFixtureNoExports
             "non-callable-oversaturation-diagnostic"
             ( Text.unlines
                 [ "identity :: Int -> Int.",
                   "identity = \\(item) -> item.",
                   "identity 1 2."
                 ]
             )
         ),
         ( "nested-scalar-capture",
           sourceFixtureNoExports
             "nested-scalar-capture"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "\\(outer) -> \\(item) -> item + outer + seed."
                 ]
             )
         ),
         ( "nested-shadow-capture-order",
           sourceFixtureNoExports
             "nested-shadow-capture-order"
             ( Text.unlines
                 [ "left :: Int.",
                   "left = 10.",
                   "right :: Int.",
                   "right = 20.",
                   "\\(outer) -> \\(left) -> right + outer + left + right."
                 ]
             )
         ),
         ( "nested-closure-valued-capture",
           sourceFixtureNoExports
             "nested-closure-valued-capture"
             "\\(predicate) -> \\(item) -> predicate (item == True) == True."
         ),
         ( "named-nested-captured-closure-call",
           sourceFixtureNoExports
             "named-nested-captured-closure-call"
             ( Text.unlines
                 [ "seed :: Bool.",
                   "seed = True.",
                   "makePredicate :: (Bool -> Bool) -> Bool -> Bool.",
                   "makePredicate = \\(predicate) -> \\(item) -> predicate item == seed.",
                   "True."
                 ]
             )
         ),
         ( "transitive-named-closure-capture",
           sourceFixtureNoExports
             "transitive-named-closure-capture"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "addSeed :: Int -> Int.",
                   "addSeed = \\(item) -> item + seed.",
                   "callAddSeed :: Int -> Int.",
                   "callAddSeed = \\(item) -> addSeed item.",
                   "callAddSeed 41."
                 ]
             )
         ),
         ( "unsupported-managed-capture",
           sourceFixtureNoExports
             "unsupported-managed-capture"
             ( Text.unlines
                 [ "message = \"managed\".",
                   "check :: Bool -> Bool.",
                   "check = \\(ignored) -> message == message.",
                   "()."
                 ]
             )
         ),
         ( "signed-function-only",
           sourceFixtureNoExports
             "signed-function-only"
             ( Text.unlines
                 [ "identity :: Int -> Int.",
                   "identity = \\(item) -> item."
                 ]
             )
         ),
         ( "missing-result-failure-accumulation",
           sourceFixtureNoExports
             "missing-result-failure-accumulation"
             ( Text.unlines
                 [ "seed :: Int.",
                   "seed = 1.",
                   "addSeed :: Int -> Int.",
                   "addSeed = \\(item) -> item + seed."
                 ]
             )
         ),
         ( "nested-unsupported-children",
           sourceFixtureNoExports
             "nested-unsupported-children"
             "if True then [1] else [2]."
         ),
         ( "pattern-case-unsupported-children",
           sourceFixtureNoExports
             "pattern-case-unsupported-children"
             "case [1] { | _ -> [2] }."
         ),
         ( "guarded-pattern-case-unsupported-children",
           sourceFixtureNoExports
             "guarded-pattern-case-unsupported-children"
             "case [1] { | _ if { ignored = [2]. True. } -> [3] | _ -> [4] }."
         ),
         ( "nested-block-unsupported-child",
           sourceFixtureNoExports
             "nested-block-unsupported-child"
             "{ ignored = [1]. [2]. }."
         ),
         ( "unsupported-binary-child",
           sourceFixtureNoExports
             "unsupported-binary-child"
             ( Text.unlines
                 [ "operator %% tier 2.",
                   "(%%) :: Int -> Int -> Int.",
                   "(%%) = \\(left, right) -> left + right.",
                   "(if True then 1 else 2) %% (if True then 3 else 4)."
                 ]
             )
         ),
         ( "left-section-unsupported-child",
           sourceFixtureNoExports
             "left-section-unsupported-child"
             "((if True then 1 else 2) +)."
         ),
         ( "right-section-unsupported-child",
           sourceFixtureNoExports
             "right-section-unsupported-child"
             "(+ (if True then 1 else 2))."
         ),
         ( "type-application-composite",
           sourceFixtureNoExports
             "type-application-composite"
             ( Text.unlines
                 [ "identity :: a -> a.",
                   "identity = \\(item) -> item.",
                   "identity @Int 1."
                 ]
             )
         ),
         ( "signed-function-rebinding",
           sourceFixtureNoExports
             "signed-function-rebinding"
             ( Text.unlines
                 [ "identity :: Int -> Int.",
                   "identity = \\(item) -> item.",
                   "identity :: Int -> Int.",
                   "identity = \\(item) -> item + 1.",
                   "identity 1."
                 ]
             )
         ),
         ( "duplicate-leading-parameters",
           sourceFixtureNoExports
             "duplicate-leading-parameters"
             ( Text.unlines
                 [ "chooseSecond :: Int -> Int -> Int.",
                   "chooseSecond = \\(item, item) -> item.",
                   "chooseSecond 1 2."
                 ]
             )
         ),
         ( "curried-shadowed-parameter",
           sourceFixtureNoExports
             "curried-shadowed-parameter"
             ( Text.unlines
                 [ "chooseSecond :: Int -> Int -> Int.",
                   "chooseSecond = \\(item) -> \\(item) -> item.",
                   "chooseSecond 1 2."
                 ]
             )
         ),
         ( "out-of-range-signed-function-literal",
           sourceFixtureNoExports
             "out-of-range-signed-function-literal"
             ( Text.unlines
                 [ "invalid :: Bool -> Int8.",
                   "invalid = \\(ignored) -> 999.",
                   "invalid True."
                 ]
             )
         ),
         ( "class-impl-declarations",
           sourceFixtureNoExports
             "class-impl-declarations"
             ( Text.unlines
                 [ "class Marker(a) { }.",
                   "impl Marker(Int) { }.",
                   "1."
                 ]
             )
         ),
         ( "impl-method-profile-failure",
           sourceFixtureNoExports
             "impl-method-profile-failure"
             ( Text.unlines
                 [ "class Items(a) { items :: a -> [Int]. }.",
                   "impl Items(Int) { items = \\(item) -> [item]. }.",
                   "()."
                 ]
             )
         ),
         ( "unsupported-binding-child-failure",
           sourceFixtureNoExports
             "unsupported-binding-child-failure"
             ( Text.unlines
                 [ "seed = [1].",
                   "()."
                 ]
             )
         ),
         ( "invalid-forward-signed-function",
           sourceFixtureNoExports
             "invalid-forward-signed-function"
             ( Text.unlines
                 [ "first :: Int -> Int.",
                   "first = \\(item) -> later item.",
                   "later :: Int -> Int.",
                   "later = \\(item) -> item True.",
                   "first 1."
                 ]
             )
         ),
         ( "qualified-method-profile-rejection",
           sourceFixtureNoExports
             "qualified-method-profile-rejection"
             ( Text.unlines
                 [ "class Choice(a) { pick :: a -> Bool. }.",
                   "impl Choice(Int) { pick = \\(candidate) -> True. }.",
                   "impl Choice(Bool) { pick = \\(candidate) -> False. }.",
                   "Choice::pick 1."
                 ]
             )
         ),
         ( "out-of-range-default-integer",
           sourceFixtureNoExports
             "out-of-range-default-integer"
             "9223372036854775808."
         ),
         ( "out-of-range-default-integer-binary",
           sourceFixtureNoExports
             "out-of-range-default-integer-binary"
             "9223372036854775807 + 1."
         ),
         ( "integer-literal-float64-promotion",
           sourceFixtureNoExports
             "integer-literal-float64-promotion"
             "1 + 2.0."
         ),
         ( "integer-literal-float64-equality",
           sourceFixtureNoExports
             "integer-literal-float64-equality"
             "1 == 2.0."
         ),
         ( "signed-parameter-float64-promotion",
           sourceFixtureNoExports
             "signed-parameter-float64-promotion"
             ( Text.unlines
                 [ "promote :: Int -> Float64 -> Float64.",
                   "promote = \\(whole, fractional) -> whole + fractional.",
                   "promote 1 2.0."
                 ]
             )
         )
       ]
