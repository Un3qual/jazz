{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.BoundaryTests where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (DataConstructor (..), Expr (..), Literal (..), Statement (..))
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.Support
import Jazz.Compiler.DiagnosticCatalog (diagnosticCodeText)
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
    diagnosticCode,
    isErrorDiagnostic,
  )
import Jazz.Compiler.LoweredIR.Lower
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.ModuleExports (ModuleExport (..), ModuleExportSelector (..), exportInventory)
import Jazz.Compiler.ModuleGraph (CoreModule (..), DeclaredModuleExports (..), ResolvedModule (..))
import Jazz.Compiler.Name (NameNamespace (ValueNamespace), operatorBindingName)
import Jazz.Compiler.TypeInference hiding (InferenceResult (..))
import Jazz.Compiler.TypeInference.Elaboration
  ( expressionDependencyNames,
    finalizeValidatedTypedCoreExpressionDirectCall,
    typedCoreProductionOutcomeStatus,
  )
import Jazz.Compiler.TypeInference.Elaboration.Types
  ( ProvisionalCallableDeclaration (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
  )
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.TypeInference.State (initialInferState)
import Jazz.Compiler.TypeInference.Types
  ( DataTypeBinding (..),
    ExpressionType (TBoolType, TFunctionType),
    ScopeCapabilityFacts (..),
    TypeBinding (PlainTypeBinding),
    emptyScopeCapabilityFacts,
  )
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
import Jazz.TestHarness (assertEqual, failTest)

testUnusedUserDefinedOperatorBinding :: IO ()
testUnusedUserDefinedOperatorBinding = do
  let fixture = producerEdgeFixture "unused-user-defined-operator"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 1)
              TypedCoreUserDefinedOperatorUnsupported
              TypedCoreUnsupportedRootDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "unused user-defined operator inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "unused user-defined operator repeatable production" firstRun secondRun
  assertEqual "unused user-defined operator binding rejection" expected (typedCoreProductionStatus firstRun)

testRootDataFailureAccumulation :: IO ()
testRootDataFailureAccumulation = do
  let fixture = producerEdgeFixture "root-data-failure-accumulation"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 0 [])
              TypedCoreStructuredValueUnsupported
              TypedCoreListValueDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "root data failure inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "root data failure repeatable production" firstRun secondRun
  assertEqual "root data failure structural order" expected (typedCoreProductionStatus firstRun)

testNestedDataFailureAccumulation :: IO ()
testNestedDataFailureAccumulation =
  case fixtures of
    fixture : _ -> do
      resolvedModule <- resolveFixtureModule fixture
      let spanValue = SourceSpan 1 1
          nestedBlock =
            EBlock
              [ SExpr spanValue (EList [ELit (LInt 1)]),
                SData spanValue "Box" [] [DataConstructor "Box" []],
                SExpr spanValue (ETuple [])
              ]
          forgedModule =
            withExpression
              (EBlock [SExpr spanValue nestedBlock])
              resolvedModule
          expected =
            TypedCoreProductionUnsupported
              [ TypedCoreProductionFailure
                  (TypedCoreProductionExpressionPath ["App", "Main"] 0 [])
                  TypedCoreStructuredValueUnsupported
                  TypedCoreDataValueDetail,
                TypedCoreProductionFailure
                  (TypedCoreProductionExpressionPath ["App", "Main"] 0 [0])
                  TypedCoreStructuredValueUnsupported
                  TypedCoreListValueDetail
              ]
      firstRun <- produceResolvedFixture fixture forgedModule
      secondRun <- produceResolvedFixture fixture forgedModule
      assertEqual "nested data failure repeatable production" firstRun secondRun
      assertEqual "nested data failure structural order" expected (typedCoreProductionStatus firstRun)
    [] -> failTest "unit fixture is missing"

testAnonymousLambdaResultAcceptance :: IO ()
testAnonymousLambdaResultAcceptance = do
  let fixture = producerEdgeFixture "anonymous-lambda-result"
      expected = lookup "anonymous-lambda-result" lexicalCaptureExpectedPrograms
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "anonymous lambda result inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "anonymous lambda result repeatable production" firstRun secondRun
  case expected of
    Just expectedProgram ->
      assertEqual "anonymous lambda module result acceptance" (TypedCoreProductionSucceeded expectedProgram) (typedCoreProductionStatus firstRun)
    Nothing -> failTest "anonymous lambda result expectation is missing"

testProductionDiagnosticCompatibility :: IO ()
testProductionDiagnosticCompatibility = do
  let fixture = producerEdgeFixture "out-of-range-signed-function-literal"
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "out-of-range signed function reports one ordinary type error"
    ["E2005"]
    [ diagnosticCodeText (diagnosticCode diagnostic)
    | diagnostic <- inferredDiagnostics ordinary,
      isErrorDiagnostic diagnostic
    ]
  assertEqual
    "out-of-range signed function inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "out-of-range signed function repeatable production" firstRun secondRun
  assertEqual
    "out-of-range signed function blocks typed-core production"
    TypedCoreProductionBlockedByDiagnostics
    (typedCoreProductionStatus firstRun)
  assertEqual
    "blocked production does not retain a validation proof"
    Nothing
    (typedCoreProductionValidatedProgram firstRun)

testInvalidForwardDeclarationAnalysisVisibility :: IO ()
testInvalidForwardDeclarationAnalysisVisibility = do
  let fixture = producerEdgeFixture "invalid-forward-signed-function"
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "invalid signed forward declaration production diagnostics"
    ["E2006"]
    [ diagnosticCodeText (diagnosticCode diagnostic)
    | diagnostic <- inferredDiagnostics (typedCoreProductionInferenceResult firstRun),
      isErrorDiagnostic diagnostic
    ]
  assertEqual "invalid signed forward declaration repeatable production" firstRun secondRun
  assertEqual
    "invalid signed forward declaration blocks typed-core production"
    TypedCoreProductionBlockedByDiagnostics
    (typedCoreProductionStatus firstRun)

testUnsupportedDeclarationProfile :: IO ()
testUnsupportedDeclarationProfile = do
  let fixture = producerEdgeFixture "class-impl-declarations"
      expected =
        TypedCoreProductionUnsupported
          [ unsupportedStatement 0,
            unsupportedStatement 1
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "class and impl declaration ordinary diagnostics"
    []
    (filter isErrorDiagnostic (inferredDiagnostics ordinary))
  assertEqual
    "class and impl declaration inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "class and impl declaration repeatable production" firstRun secondRun
  assertEqual "class and impl declaration failures" expected (typedCoreProductionStatus firstRun)
  where
    unsupportedStatement statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail

testImplMethodBodyFailureAccumulation :: IO ()
testImplMethodBodyFailureAccumulation = do
  let fixture = producerEdgeFixture "impl-method-profile-failure"
      expected =
        TypedCoreProductionUnsupported
          [ unsupportedStatement 0,
            unsupportedStatement 1,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 1 [0, 0])
              TypedCoreStructuredValueUnsupported
              TypedCoreListValueDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "impl method body ordinary diagnostics"
    []
    (filter isErrorDiagnostic (inferredDiagnostics ordinary))
  assertEqual
    "impl method body inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "impl method body repeatable production" firstRun secondRun
  assertEqual "impl method body complete failure accumulation" expected (typedCoreProductionStatus firstRun)
  where
    unsupportedStatement statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail

testUnsupportedBindingFailureAccumulation :: IO ()
testUnsupportedBindingFailureAccumulation = do
  let fixture = producerEdgeFixture "unsupported-binding-child-failure"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 0)
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 0 [0])
              TypedCoreStructuredValueUnsupported
              TypedCoreListValueDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "unsupported binding ordinary diagnostics"
    []
    (filter isErrorDiagnostic (inferredDiagnostics ordinary))
  assertEqual
    "unsupported binding inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "unsupported binding repeatable production" firstRun secondRun
  assertEqual "unsupported binding complete failure accumulation" expected (typedCoreProductionStatus firstRun)

testQualifiedMethodInferenceCompatibility :: IO ()
testQualifiedMethodInferenceCompatibility = do
  let fixture = producerEdgeFixture "qualified-method-profile-rejection"
      expected =
        TypedCoreProductionUnsupported
          [ unsupportedStatement 0,
            unsupportedStatement 1,
            unsupportedStatement 2,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 3 [])
              TypedCoreNonLocalCallUnsupported
              (TypedCoreNameDetail "Choice::pick")
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "qualified method ordinary diagnostics"
    []
    (filter isErrorDiagnostic (inferredDiagnostics ordinary))
  assertEqual
    "qualified method inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "qualified method repeatable production" firstRun secondRun
  assertEqual "qualified method profile failures" expected (typedCoreProductionStatus firstRun)
  where
    unsupportedStatement statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail

testOutOfRangeDefaultIntegerRejection :: IO ()
testOutOfRangeDefaultIntegerRejection =
  mapM_ assertRejected ["out-of-range-default-integer", "out-of-range-default-integer-binary"]
  where
    assertRejected name = do
      let fixture = producerEdgeFixture name
          expected =
            TypedCoreProductionUnsupported
              [ TypedCoreProductionFailure
                  (TypedCoreProductionExpressionPath ["App", "Main"] 0 [])
                  TypedCoreUnresolvedExpressionType
                  TypedCoreUnsupportedRootDetail
              ]
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        (filter isErrorDiagnostic (inferredDiagnostics ordinary))
      assertEqual
        (name <> " inference compatibility")
        ordinary
        (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual (name <> " structured rejection") expected (typedCoreProductionStatus firstRun)

testNumericPromotionRejection :: IO ()
testNumericPromotionRejection =
  mapM_
    assertRejected
    [ ("integer-literal-float64-promotion", 0, []),
      ("integer-literal-float64-equality", 0, []),
      ("signed-parameter-float64-promotion", 1, [0, 0, 0])
    ]
  where
    assertRejected (name, statementIndex, childPath) = do
      let fixture = producerEdgeFixture name
          expected =
            TypedCoreProductionUnsupported
              [ TypedCoreProductionFailure
                  (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
                  TypedCoreUnsupportedRootExpression
                  TypedCoreUnsupportedRootDetail
              ]
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        (filter isErrorDiagnostic (inferredDiagnostics ordinary))
      assertEqual
        (name <> " inference compatibility")
        ordinary
        (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual (name <> " structured rejection") expected (typedCoreProductionStatus firstRun)

testRejectedCallableProfile :: IO ()
testRejectedCallableProfile =
  mapM_ assertRejected callableExpectedStatuses
    >> assertEqual
      "callable rejection failure-kind coverage"
      True
      ( all (`elem` actualKinds) expectedKinds
          && all (`elem` expectedKinds) actualKinds
      )
  where
    expectedKinds =
      [ TypedCoreImportedInputsUnsupported,
        TypedCoreUserDefinedOperatorUnsupported,
        TypedCoreNonMonomorphicFunctionUnsupported,
        TypedCoreUnresolvedExpressionType
      ]
    actualKinds =
      [ kind
      | (_, TypedCoreProductionUnsupported failures) <- callableExpectedStatuses,
        TypedCoreProductionFailure _ kind _ <- failures
      ]
    assertRejected (name, expectedStatus) = do
      let fixture = fixtureByName name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " repeatable rejection") firstRun secondRun
      assertEqual (name <> " rejection inference compatibility") ordinary (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " complete rejection") expectedStatus (typedCoreProductionStatus firstRun)

callableExpectedStatuses :: [(Text, TypedCoreProductionStatus)]
callableExpectedStatuses =
  map expectedStatus callableRejectionNames
  where
    expectedStatus name =
      case lookup name rejectedManifestExpectedStatuses of
        Just status -> (name, status)
        Nothing -> error ("callable rejection is missing from the rejected manifest: " <> show name)

callableRejectionNames :: [Text]
callableRejectionNames =
  [ "oversaturated-direct-call",
    "polymorphic-or-evidence-function",
    "imported-direct-call",
    "user-defined-operator-call"
  ]

testNestedScalarLowering :: IO ()
testNestedScalarLowering =
  assertEqual
    "nested scalar lowering"
    (LoweredIRSucceeded expectedNestedScalarLoweredProgram)
    (lowerTypedCoreExpressionDirectCall nestedScalarTypedProgram)

testExplicitNumericWidthLowering :: IO ()
testExplicitNumericWidthLowering =
  mapM_
    ( \(name, typedProgram, expectedProgram) ->
        let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
            secondRun = lowerTypedCoreExpressionDirectCall typedProgram
         in assertEqual (name <> " repeatable lowering") firstRun secondRun
              >> assertEqual
                (name <> " exact lowering")
                (LoweredIRSucceeded expectedProgram)
                firstRun
    )
    explicitNumericScalarLoweringPrograms

testFullUInt64Lowering :: IO ()
testFullUInt64Lowering =
  mapM_
    ( \(name, typedProgram, expectedProgram) ->
        let firstRun = lowerTypedCoreExpressionDirectCall typedProgram
            secondRun = lowerTypedCoreExpressionDirectCall typedProgram
         in assertEqual (name <> " permanent validation") [] (validateLoweredProgram expectedProgram)
              >> assertEqual (name <> " repeatable lowering") firstRun secondRun
              >> assertEqual
                (name <> " exact lowering")
                (LoweredIRSucceeded expectedProgram)
                firstRun
    )
    fullUInt64ScalarLoweringPrograms

testLoweringPrecedence :: IO ()
testLoweringPrecedence =
  assertEqual
    "typed-core validation precedes lowering profile checks"
    ( LoweredIRTypedCoreFailures
        [ TypedCoreValidationFailure
            TypedProgramPath
            TypedUnknownEntryModule
            (TypedTextDetail "Missing::Entry")
        ]
    )
    (lowerTypedCoreExpressionDirectCall (TypedProgram Nothing [] ["Missing", "Entry"]))

testRejectedScalarProfile :: IO ()
testRejectedScalarProfile =
  mapM_ assertRejected rejectedScalarFixtures
  where
    assertRejected fixture = do
      let name = fixtureName fixture
          expectedStatus =
            case lookup name rejectedManifestExpectedStatuses of
              Just status -> status
              Nothing -> error ("scalar rejection is missing from the rejected manifest: " <> show name)
      ordinary <- inferFixture fixture
      firstResult <- produceFixture fixture
      secondResult <- produceFixture fixture
      let firstRun = typedCoreProductionStatus firstResult
          secondRun = typedCoreProductionStatus secondResult
      assertEqual (name <> " repeatable rejection") firstRun secondRun
      assertEqual (name <> " scalar rejection inference compatibility") ordinary (typedCoreProductionInferenceResult firstResult)
      assertEqual (name <> " production failure") expectedStatus firstRun

testMissingModuleResultProduction :: IO ()
testMissingModuleResultProduction =
  mapM_ assertMissing ["empty-module", "signed-function-only"]
  where
    expected =
      TypedCoreProductionUnsupported
        [ TypedCoreProductionFailure
            (TypedCoreProductionModulePath ["App", "Main"])
            TypedCoreUnsupportedRootExpression
            TypedCoreUnsupportedRootDetail
        ]
    assertMissing name = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " ordinary diagnostics") [] (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual (name <> " exact missing-result failure") expected (typedCoreProductionStatus firstRun)

testMissingResultFailureAccumulation :: IO ()
testMissingResultFailureAccumulation = do
  let fixture = producerEdgeFixture "missing-result-failure-accumulation"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionModulePath ["App", "Main"])
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "missing-result failure inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "missing-result failure repeatability" firstRun secondRun
  assertEqual "missing-result complete failure accumulation" expected (typedCoreProductionStatus firstRun)

testModuleFailureOrder :: IO ()
testModuleFailureOrder = do
  let fixture = fixtureByName "unit-entry"
      selectors =
        [ ModuleExportSelector (Just ValueNamespace) "zeta",
          ModuleExportSelector (Just ValueNamespace) "alpha"
        ]
      inventory =
        exportInventory
          [ ModuleExport ValueNamespace "zeta",
            ModuleExport ValueNamespace "alpha"
          ]
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionModulePath ["App", "Main"])
              TypedCoreUnsupportedRootExpression
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionModulePath ["App", "Main"])
              TypedCoreUnsupportedExport
              (TypedCoreNameDetail "zeta"),
            TypedCoreProductionFailure
              (TypedCoreProductionModulePath ["App", "Main"])
              TypedCoreUnsupportedExport
              (TypedCoreNameDetail "alpha")
          ]
  resolvedModule <- resolveFixtureModule fixture
  let coreModule = resolvedModuleCore resolvedModule
      mutatedModule =
        resolvedModule
          { resolvedModuleExportInventory = inventory,
            resolvedModuleCore =
              coreModule
                { coreModuleDeclaredExports =
                    Just (DeclaredModuleExports (SourceSpan 1 1) selectors),
                  coreModuleExpr = EBlock [SLet "ignored" (SourceSpan 2 1) (ETuple [])]
                }
          }
  firstRun <- produceResolvedFixture fixture mutatedModule
  secondRun <- produceResolvedFixture fixture mutatedModule
  assertEqual "module failure order repeatability" firstRun secondRun
  assertEqual "module failures precede statements in authored export order" expected (typedCoreProductionStatus firstRun)

testExportedCallableFailureOwnership :: IO ()
testExportedCallableFailureOwnership = do
  let fixture = producerEdgeFixture "default-exported-polymorphic-callable"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionModulePath ["App", "Main"])
              TypedCoreUnsupportedExport
              (TypedCoreNameDetail "seed"),
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 2 [])
              TypedCoreUnresolvedExpressionType
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 3)
              TypedCoreNonMonomorphicFunctionUnsupported
              (TypedCoreNameDetail "identity"),
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 3 [])
              TypedCoreUnresolvedExpressionType
              TypedCoreUnsupportedRootDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 3 [0])
              TypedCoreUnresolvedExpressionType
              TypedCoreUnsupportedRootDetail
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "default-exported polymorphic callable inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "default-exported polymorphic callable repeatability" firstRun secondRun
  assertEqual "export callable failure remains statement-owned and unique" expected (typedCoreProductionStatus firstRun)

testCompoundFailureAccumulation :: IO ()
testCompoundFailureAccumulation = do
  let fixture = producerEdgeFixture "nested-unsupported-children"
      expected =
        TypedCoreProductionUnsupported
          [ expressionFailure [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure [2] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
          ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "compound failure ordinary diagnostics" [] (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
  assertEqual "compound failure repeatable production" firstRun secondRun
  assertEqual "compound failure structural preorder" expected (typedCoreProductionStatus firstRun)
  where
    expressionFailure childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] 0 childPath)
        kind
        detail

testUnsupportedCompositeFailureAccumulation :: IO ()
testUnsupportedCompositeFailureAccumulation =
  mapM_ assertCompositeFailure expectedResults
  where
    expectedResults =
      [ ( "pattern-case-unsupported-children",
          [ expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [1, 1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [] TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail
          ]
        ),
        ( "nested-block-unsupported-child",
          [ expressionFailure 0 [] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail,
            expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail
          ]
        ),
        ( "guarded-pattern-case-unsupported-children",
          [ expressionFailure 0 [0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [1, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail,
            expressionFailure 0 [1, 0, 0] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [1, 1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [2, 1] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail,
            expressionFailure 0 [] TypedCorePatternCaseUnsupported TypedCorePatternCaseDetail
          ]
        ),
        ( "unsupported-binary-child",
          [ statementFailure 1 TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
            expressionFailure 2 [] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail
          ]
        ),
        ( "left-section-unsupported-child",
          [expressionFailure 0 [] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail]
        ),
        ( "right-section-unsupported-child",
          [expressionFailure 0 [] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail]
        ),
        ( "type-application-composite",
          [ expressionFailure 0 [] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail,
            statementFailure 1 TypedCoreNonMonomorphicFunctionUnsupported (TypedCoreNameDetail "identity"),
            expressionFailure 1 [] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail,
            expressionFailure 1 [0] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail,
            expressionFailure 2 [0] TypedCoreManagedValueUnsupported TypedCoreUnsupportedRootDetail
          ]
        )
      ]

    assertCompositeFailure (name, expectedFailures) = do
      let fixture = producerEdgeFixture name
      ordinary <- inferFixture fixture
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary inference compatibility")
        ordinary
        (typedCoreProductionInferenceResult firstRun)
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " structural preorder")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)

    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail

    statementFailure statementIndex kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        kind
        detail

testProducerIdentityBoundary :: IO ()
testProducerIdentityBoundary =
  mapM_ assertIdentity expectedResults
  where
    expectedResults =
      [ ( "signed-function-rebinding",
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 3)
              TypedCoreFunctionRebindingUnsupported
              (TypedCoreNameDetail "identity")
          ]
        ),
        ( "duplicate-leading-parameters",
          [parameterFailure [0, 0]]
        ),
        ( "curried-shadowed-parameter",
          [parameterFailure [0, 0]]
        )
      ]
    parameterFailure childPath =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] 1 childPath)
        TypedCoreDuplicateParameterUnsupported
        (TypedCoreNameDetail "item")
    assertIdentity (name, expectedFailures) = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual (name <> " ordinary diagnostics") [] (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact identity failure")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)

testIncompleteRecursiveGroupOwnership :: IO ()
testIncompleteRecursiveGroupOwnership = do
  resolvedModule <- resolveFixtureModule (fixtureByName "unit-entry")
  let spanValue = SourceSpan 1 1
      functionType = TFunctionType TBoolType TBoolType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [1, 3])
      loopExpression =
        ProvisionalLambdaExpression
          "item"
          functionType
          ( ProvisionalApplyExpression
              TBoolType
              (ProvisionalVariableExpression "loop" functionType)
              (ProvisionalVariableExpression "item" TBoolType)
          )
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalFunctionBinding loopDeclaration loopExpression,
            ProvisionalTerminalExpression 2 spanValue (ProvisionalLiteralExpression (LBool True) TBoolType)
          ]
      status =
        typedCoreProductionOutcomeStatus
          ( finalizeValidatedTypedCoreExpressionDirectCall
              (TypedSourcePath "src/App/Main.jz")
              resolvedModule
              initialInferState
              provisionalScope
          )
  assertEqual
    "missing recursive declaration owner rejects the complete group"
    ( TypedCoreProductionUnsupported
        [ TypedCoreProductionFailure
            (TypedCoreProductionStatementPath ["App", "Main"] 1)
            TypedCoreRecursiveFunctionUnsupported
            (TypedCoreNameDetail "loop")
        ]
    )
    status

testSameStatementFailureKindOrder :: IO ()
testSameStatementFailureKindOrder = do
  let fixture = producerEdgeFixture "self-recursive-function-rebinding"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 3)
              TypedCoreFunctionRebindingUnsupported
              (TypedCoreNameDetail "loop")
          ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "self-recursive rebinding ordinary diagnostics" [] (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
  assertEqual "self-recursive rebinding repeatability" firstRun secondRun
  assertEqual "same-statement failures follow declared kind order" expected (typedCoreProductionStatus firstRun)

testInterveningScalarCanonicalOwnership :: Text -> IO ()
testInterveningScalarCanonicalOwnership requestedName =
  case lookup requestedName expectedResults of
    Just expectedFailures -> assertExact requestedName expectedFailures
    Nothing -> error ("intervening scalar canonical ownership fixture has no expected result: " <> Text.unpack requestedName)
  where
    expectedResults =
      [ ( "intervening-scalar-canonical-ownership",
          [rootFailure 2]
        ),
        ( "multiple-intervening-scalars-canonical-ownership",
          [ rootFailure 2,
            rootFailure 3
          ]
        ),
        ( "interleaved-callable-scalar-canonical-ownership",
          [ rootFailure 2,
            rebindingFailure 4 "a",
            rootFailure 5
          ]
        )
      ]
    assertExact name expectedFailures = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        ( filter
            isErrorDiagnostic
            (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
        )
      assertEqual (name <> " repeatability") firstRun secondRun
      assertEqual
        (name <> " exact canonical scalar-owner result")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)
    rootFailure statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail
    rebindingFailure statementIndex name =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreFunctionRebindingUnsupported
        (TypedCoreNameDetail name)

testCanonicalRecursionTransportControls :: IO ()
testCanonicalRecursionTransportControls =
  mapM_
    assertOwners
    [ ("self-recursive-function-rebinding", []),
      ("three-same-name-nearest-prior-mutual-recursion", [(5, "identity"), (7, "peer")]),
      ("canonical-self-recursion-no-prior", []),
      ("canonical-mutual-recursion-peers", []),
      ("rejected-self-alias-recursion", [(1, "loop")]),
      ("rejected-mutual-alias-recursion", [(1, "left"), (3, "right")]),
      ("rejected-alias-conditional-mutual-recursion", [(1, "left"), (3, "right")]),
      ("rejected-operator-alias-self-recursion", [(1, "$operator:%25%25")]),
      ("rejected-conditional-self-recursion", []),
      ("rejected-block-conditional-mutual-recursion", [(1, "left"), (3, "right")]),
      ("rejected-block-later-shadow-control", [(1, "loop")]),
      ("rejected-block-initializer-self-recursion", [(1, "loop")]),
      ("rejected-block-initializer-mutual-recursion", [(1, "left"), (3, "right")]),
      ("rejected-block-later-signed-shadow-control", [(1, "loop")]),
      ("rejected-operator-value-self-recursion", [(1, "$operator:%25%25")]),
      ("rejected-infix-operator-mutual-recursion", [(1, "$operator:%25%25"), (3, "$operator:%7E%7E")]),
      ("rejected-section-operator-mutual-recursion", [(1, "$operator:%25%25"), (3, "$operator:%7E%7E")])
    ]
  where
    assertOwners (name, expectedOwners) = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " canonical transport ordinary diagnostics")
        []
        ( filter
            isErrorDiagnostic
            (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
        )
      assertEqual (name <> " canonical transport repeatability") firstRun secondRun
      assertEqual
        (name <> " exact recursive owner order and multiplicity")
        expectedOwners
        (recursiveOwners (typedCoreProductionStatus firstRun))
    recursiveOwners status =
      case status of
        TypedCoreProductionUnsupported failures ->
          [ (statementIndex, name)
          | TypedCoreProductionFailure
              (TypedCoreProductionStatementPath _ statementIndex)
              TypedCoreRecursiveFunctionUnsupported
              (TypedCoreNameDetail name) <-
              failures
          ]
        _ -> []

testNestedPriorOuterAliasOwnership :: Text -> IO ()
testNestedPriorOuterAliasOwnership requestedName = do
  let fixture = producerEdgeFixture requestedName
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    (requestedName <> " ordinary diagnostics")
    []
    ( filter
        isErrorDiagnostic
        (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
    )
  assertEqual (requestedName <> " repeatability") firstRun secondRun
  assertEqual
    (requestedName <> " exact prior-outer recursive owner order and multiplicity")
    [(1, "left"), (3, "right")]
    (recursiveOwners (typedCoreProductionStatus firstRun))
  where
    recursiveOwners status =
      case status of
        TypedCoreProductionUnsupported failures ->
          [ (statementIndex, name)
          | TypedCoreProductionFailure
              (TypedCoreProductionStatementPath _ statementIndex)
              TypedCoreRecursiveFunctionUnsupported
              (TypedCoreNameDetail name) <-
              failures
          ]
        _ -> []

testNestedSelfRecursiveLambdaOwnership :: IO ()
testNestedSelfRecursiveLambdaOwnership = do
  let fixture = producerEdgeFixture "nested-self-recursive-lambda-local-ownership"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 1 [0, 0])
              TypedCoreNestedBlockUnsupported
              TypedCoreLocalBlockDetail
          ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "nested self-recursive lambda ordinary diagnostics"
    []
    ( filter
        isErrorDiagnostic
        (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
    )
  assertEqual "nested self-recursive lambda repeatability" firstRun secondRun
  assertEqual "nested self-recursive lambda exact local ownership" expected (typedCoreProductionStatus firstRun)

testNestedLambdaRecursiveAdmission :: IO ()
testNestedLambdaRecursiveAdmission = do
  let fixture = producerEdgeFixture "nested-lambda-direct-recursion"
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 3)
              TypedCoreRecursiveFunctionUnsupported
              (TypedCoreNameDetail "loop")
          ]
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual
    "nested-lambda direct recursion ordinary diagnostics"
    []
    (filter isErrorDiagnostic (inferredDiagnostics ordinary))
  assertEqual
    "nested-lambda direct recursion inference compatibility"
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual "nested-lambda direct recursion repeatability" firstRun secondRun
  assertEqual "nested-lambda direct recursion producer rejection" expected (typedCoreProductionStatus firstRun)

testRejectedCallableRebinding :: Text -> IO ()
testRejectedCallableRebinding requestedName =
  case lookup requestedName expectedResults of
    Just expectedFailures -> assertExact requestedName expectedFailures
    Nothing -> error ("rejected callable rebinding fixture has no expected result: " <> Text.unpack requestedName)
  where
    expectedResults =
      [ ( "accepted-then-rejected-callable-rebinding",
          [ rebindingFailure 3,
            rootFailure 3
          ]
        ),
        ( "rejected-recursive-callable-rebinding-order",
          [ recursionFailure 3 "f",
            rebindingFailure 3,
            rootFailure 3,
            recursionFailure 5 "g"
          ]
        ),
        ( "rejected-then-accepted-callable-rebinding",
          [ rootFailure 1,
            rebindingFailure 3
          ]
        ),
        ( "repeated-rejected-callable-rebinding",
          [ rootFailure 1,
            rebindingFailure 3,
            rootFailure 3
          ]
        ),
        ( "scalar-then-rejected-callable-control",
          [rootFailure 2]
        ),
        ( "accepted-scalar-rejected-callable-rebinding",
          [ rootFailure 2,
            rebindingFailure 4,
            rootFailure 4
          ]
        ),
        ( "rejected-scalar-accepted-callable-rebinding",
          [ rootFailure 1,
            rootFailure 2,
            rebindingFailure 4
          ]
        )
      ]
    assertExact name expectedFailures = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        ( filter
            isErrorDiagnostic
            (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
        )
      assertEqual (name <> " repeatability") firstRun secondRun
      assertEqual
        (name <> " exact callable rebinding result")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)
    rebindingFailure statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreFunctionRebindingUnsupported
        (TypedCoreNameDetail "f")
    recursionFailure statementIndex name =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreRecursiveFunctionUnsupported
        (TypedCoreNameDetail name)
    rootFailure statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail

testCanonicalCallableRebindingDependencies :: Text -> IO ()
testCanonicalCallableRebindingDependencies requestedName =
  case lookup requestedName acceptedGroups of
    Just groups -> assertAccepted requestedName groups
    Nothing ->
      case lookup requestedName expectedResults of
        Just expectedFailures -> assertExact requestedName expectedFailures
        Nothing -> error ("canonical callable rebinding fixture has no expected result: " <> Text.unpack requestedName)
  where
    acceptedGroups =
      [ ("canonical-self-recursion-no-prior", [[(1, "loop")]]),
        ("canonical-mutual-recursion-peers", [[(1, "left"), (3, "right")]])
      ]
    expectedResults =
      [ ( "later-callable-rebinding-calls-nearest-prior",
          [rebindingFailure 3 "identity"]
        ),
        ( "three-same-name-nearest-prior-mutual-recursion",
          [ rebindingFailure 3 "identity",
            recursionFailure 5 "identity",
            rebindingFailure 5 "identity",
            recursionFailure 7 "peer"
          ]
        ),
        ( "nearest-rebinding-mutual-control",
          [rebindingFailure 5 "left"]
        ),
        ( "rebinding-parameter-shadow-control",
          [rebindingFailure 3 "apply"]
        ),
        ( "rebinding-local-shadow-control",
          [ rebindingFailure 3 "loop",
            expressionFailure 3 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
          ]
        )
      ]
    assertExact name expectedFailures = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        ( filter
            isErrorDiagnostic
            (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
        )
      assertEqual (name <> " repeatability") firstRun secondRun
      assertEqual
        (name <> " exact canonical owners, order, and multiplicity")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)
    assertAccepted name expectedGroups = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
      assertEqual (name <> " repeatability") firstRun secondRun
      case typedCoreProductionStatus firstRun of
        TypedCoreProductionSucceeded program -> do
          assertEqual (name <> " accepted recursive validation") [] (validateTypedProgram program)
          assertEqual (name <> " exact typed recursive groups") expectedGroups (typedRecursiveGroupOwners program)
        status -> failTest (name <> " did not produce typed recursion: " <> Text.pack (show status))
    typedRecursiveGroupOwners program =
      case program of
        TypedProgram _ [TypedModule _ _ _ _ _ groups _ _] _ ->
          [ [ (statementIndex, name)
            | TypedBinderId (_, statementIndex : _, TypedResolvedName _ _ name) <- members
            ]
          | TypedRecursiveGroup members <- groups
          ]
        _ -> error "canonical direct recursion expected one typed module"
    recursionFailure statementIndex name =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreRecursiveFunctionUnsupported
        (TypedCoreNameDetail name)
    rebindingFailure statementIndex name =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreFunctionRebindingUnsupported
        (TypedCoreNameDetail name)
    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail

testRejectedCallableDeclarationTransport :: Text -> IO ()
testRejectedCallableDeclarationTransport requestedName =
  case lookup requestedName expectedResults of
    Just expectedFailures -> assertExact requestedName expectedFailures
    Nothing -> error ("rejected callable declaration fixture has no expected result: " <> Text.unpack requestedName)
  where
    expectedResults =
      [ ( "rejected-self-alias-recursion",
          [ recursionFailure 1 "loop",
            rootFailure 1
          ]
        ),
        ( "rejected-mutual-alias-recursion",
          [ recursionFailure 1 "left",
            rootFailure 1,
            recursionFailure 3 "right",
            rootFailure 3
          ]
        ),
        ( "rejected-alias-conditional-mutual-recursion",
          [ recursionFailure 1 "left",
            rootFailure 1,
            recursionFailure 3 "right",
            rootFailure 3
          ]
        ),
        ( "rejected-operator-alias-self-recursion",
          [ recursionFailure 1 "$operator:%25%25",
            rootFailure 1,
            expressionFailure 1 [0] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail
          ]
        ),
        ( "rejected-eager-operator-conditional-control",
          [ rootFailure 1,
            expressionFailure 1 [0, 0] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
            expressionFailure 1 [0, 1] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
            expressionFailure 1 [0, 2] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail
          ]
        ),
        ( "rejected-alias-parameter-shadow-control",
          [rootFailure 3]
        ),
        ( "rejected-alias-local-shadow-control",
          [ rootFailure 1,
            expressionFailure 1 [0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
          ]
        ),
        ( "rejected-eager-self-before-callable-result-control",
          [ rootFailure 1,
            expressionFailure 1 [0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
          ]
        ),
        ( "rejected-block-nearest-prior-callable-rebinding-recursion",
          [ recursionFailure 1 "f",
            rootFailure 1,
            expressionFailure 1 [0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
          ]
        )
      ]
    assertExact name expectedFailures = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        ( filter
            isErrorDiagnostic
            (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
        )
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact declaration-owned rejection")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)
    recursionFailure statementIndex name =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreRecursiveFunctionUnsupported
        (TypedCoreNameDetail name)
    rootFailure statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUnsupportedRootExpression
        TypedCoreUnsupportedRootDetail
    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail

testRejectedProducerDependencyTransport :: IO ()
testRejectedProducerDependencyTransport =
  mapM_
    assertExact
    [ ( "rejected-block-conditional-mutual-recursion",
        [ statementFailure 1 "left",
          expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail,
          statementFailure 3 "right"
        ]
      ),
      ( "rejected-block-parameter-shadow-control",
        [expressionFailure 3 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail]
      ),
      ( "rejected-block-later-shadow-control",
        [ statementFailure 1 "loop",
          expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
        ]
      ),
      ( "rejected-block-initializer-self-recursion",
        [ statementFailure 1 "loop",
          expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
        ]
      ),
      ( "rejected-block-initializer-mutual-recursion",
        [ statementFailure 1 "left",
          expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail,
          statementFailure 3 "right",
          expressionFailure 3 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
        ]
      ),
      ( "rejected-block-later-signed-shadow-control",
        [ statementFailure 1 "loop",
          expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail
        ]
      ),
      ( "rejected-block-local-shadow-cycle-control",
        [expressionFailure 3 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail]
      ),
      ( "rejected-block-parameter-shadow-cycle-control",
        [expressionFailure 1 [0, 0] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail]
      ),
      ( "rejected-operator-value-self-recursion",
        [ generatedOperatorFailure 1,
          statementFailure 1 "$operator:%25%25",
          userOperatorFailure 1 [0, 0, 0, 0, 0]
        ]
      ),
      ( "rejected-infix-operator-mutual-recursion",
        [ generatedOperatorFailure 1,
          statementFailure 1 "$operator:%25%25",
          userOperatorFailure 1 [0, 0, 0],
          generatedOperatorFailure 3,
          statementFailure 3 "$operator:%7E%7E",
          userOperatorFailure 3 [0, 0, 0]
        ]
      ),
      ( "rejected-section-operator-mutual-recursion",
        [ generatedOperatorFailure 1,
          statementFailure 1 "$operator:%25%25",
          userOperatorFailure 1 [0, 0, 0, 0],
          generatedOperatorFailure 3,
          statementFailure 3 "$operator:%7E%7E",
          userOperatorFailure 3 [0, 0, 0, 0]
        ]
      )
    ]
  where
    assertExact (name, expectedFailures) = do
      let fixture = producerEdgeFixture name
      firstRun <- produceFixture fixture
      secondRun <- produceFixture fixture
      assertEqual
        (name <> " ordinary diagnostics")
        []
        ( filter
            isErrorDiagnostic
            (inferredDiagnostics (typedCoreProductionInferenceResult firstRun))
        )
      assertEqual (name <> " repeatable production") firstRun secondRun
      assertEqual
        (name <> " exact rejected-tree recursion result")
        (TypedCoreProductionUnsupported expectedFailures)
        (typedCoreProductionStatus firstRun)
    statementFailure statementIndex name =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreRecursiveFunctionUnsupported
        (TypedCoreNameDetail name)
    generatedOperatorFailure statementIndex =
      TypedCoreProductionFailure
        (TypedCoreProductionStatementPath ["App", "Main"] statementIndex)
        TypedCoreUserDefinedOperatorUnsupported
        TypedCoreUnsupportedRootDetail
    userOperatorFailure statementIndex childPath =
      expressionFailure
        statementIndex
        childPath
        TypedCoreUserDefinedOperatorUnsupported
        TypedCoreUnsupportedRootDetail
    expressionFailure statementIndex childPath kind detail =
      TypedCoreProductionFailure
        (TypedCoreProductionExpressionPath ["App", "Main"] statementIndex childPath)
        kind
        detail

testOperatorDependencyNames :: IO ()
testOperatorDependencyNames = do
  let userOperator = operatorBindingName "%%"
      literal = ELit (LInt 1)
  assertEqual
    "operator value dependency"
    (Set.singleton userOperator)
    (expressionDependencyNames (EOperatorValue "%%"))
  assertEqual
    "infix operator dependency"
    (Set.singleton userOperator)
    (expressionDependencyNames (EBinary "%%" literal literal))
  assertEqual
    "left section operator dependency"
    (Set.singleton userOperator)
    (expressionDependencyNames (ESectionLeft literal "%%"))
  assertEqual
    "right section operator dependency"
    (Set.singleton userOperator)
    (expressionDependencyNames (ESectionRight "%%" literal))
  assertEqual
    "builtin operator forms are dependency free"
    Set.empty
    ( foldMap
        expressionDependencyNames
        [ EOperatorValue "+",
          EBinary "+" literal literal,
          ESectionLeft literal "+",
          ESectionRight "+" literal
        ]
    )

testDiagnosticPrecedence :: IO ()
testDiagnosticPrecedence = do
  let sourceDiagnosticFixture = fixtureByName "source-diagnostic"
  ordinary <- inferFixture sourceDiagnosticFixture
  sourceDiagnosticResult <-
    inferResolvedModuleTypedCoreExpressionDirectCall
      (fixtureInputs sourceDiagnosticFixture)
      (fixtureSourcePath sourceDiagnosticFixture)
      =<< resolveFixtureModule sourceDiagnosticFixture
  assertEqual "diagnostics take precedence" TypedCoreProductionBlockedByDiagnostics (typedCoreProductionStatus sourceDiagnosticResult)
  assertEqual "diagnostic inference compatibility" ordinary (typedCoreProductionInferenceResult sourceDiagnosticResult)

testInputFailures :: IO ()
testInputFailures =
  case map fixtureByName ["invalid-portable-source-path", "resolved-import", "ambient-prelude-input"] of
    [invalidPathFixture, importFixture, ambientFixture] -> do
      assertUnsupported
        invalidPathFixture
        [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreInvalidPortableSourcePath TypedCoreNoFailureDetail]
      assertUnsupported
        importFixture
        [TypedCoreProductionFailure (TypedCoreProductionModulePath ["App", "Main"]) TypedCoreResolvedImportsUnsupported TypedCoreNoFailureDetail]
      assertUnsupported
        ambientFixture
        [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreAmbientPreludeInputUnsupported TypedCoreNoFailureDetail]
    _ -> failTest "foundation input fixtures are missing"

testInputModuleFailureOrder :: IO ()
testInputModuleFailureOrder = do
  let fixture = fixtureByName "resolved-import"
      combinedFixture =
        fixture
          { fixtureSourcePath = TypedSourcePath "/private/host/Main.jz",
            fixtureInputs =
              (fixtureInputs fixture)
                { inferenceCurrentModulePath = Just ["Other", "Main"],
                  inferenceImportedTypes = Map.singleton "foreign" (PlainTypeBinding TBoolType),
                  inferenceImportedClassNames = Set.singleton "PreludeClass"
                }
          }
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreModulePathMismatch TypedCoreNoFailureDetail,
            TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreInvalidPortableSourcePath TypedCoreNoFailureDetail,
            TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreImportedInputsUnsupported TypedCoreNoFailureDetail,
            TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreAmbientPreludeInputUnsupported TypedCoreNoFailureDetail,
            TypedCoreProductionFailure
              (TypedCoreProductionModulePath ["App", "Main"])
              TypedCoreResolvedImportsUnsupported
              TypedCoreNoFailureDetail
          ]
  firstRun <- produceFixture combinedFixture
  secondRun <- produceFixture combinedFixture
  assertEqual "input/module failure order repeatability" firstRun secondRun
  assertEqual "all input failures precede module failures" expected (typedCoreProductionStatus firstRun)

assertUnsupported :: Fixture -> [TypedCoreProductionFailure] -> IO ()
assertUnsupported fixture expectedFailures = do
  result <- produceFixture fixture
  assertEqual
    (fixtureName fixture <> " production status")
    (TypedCoreProductionUnsupported expectedFailures)
    (typedCoreProductionStatus result)

testAdditionalProfileFailures :: IO ()
testAdditionalProfileFailures =
  case fixtures of
    unitFixture : _ -> do
      resolvedUnitModule <- resolveFixtureModule unitFixture
      let pathMismatch =
            unitFixture
              { fixtureInputs = (fixtureInputs unitFixture) {inferenceCurrentModulePath = Just ["Other", "Main"]}
              }
          importedValue =
            unitFixture
              { fixtureInputs =
                  (fixtureInputs unitFixture)
                    { inferenceImportedTypes = Map.singleton "foreign" (PlainTypeBinding TBoolType)
                    }
              }
          importedData =
            unitFixture
              { fixtureInputs =
                  (fixtureInputs unitFixture)
                    { inferenceImportedDataTypes = Map.singleton "Foreign" (DataTypeBinding [] [])
                    }
              }
          importedCapabilities =
            unitFixture
              { fixtureInputs =
                  (fixtureInputs unitFixture)
                    { inferenceImportedCapabilities = emptyScopeCapabilityFacts {scopeClassFacts = Map.singleton "Foreign" 0}
                    }
              }
          unsupportedRoot = withExpression (ELit (LBool True)) resolvedUnitModule
          nonLocalCall =
            withExpression
              ( EBlock
                  [ SExpr
                      (SourceSpan 1 1)
                      (EApply (EVar "__kernel_toInt8") (ELit (LInt 1)))
                  ]
              )
              resolvedUnitModule
          unsupportedExport =
            resolvedUnitModule
              { resolvedModuleExportInventory =
                  exportInventory [ModuleExport ValueNamespace "missing"],
                resolvedModuleCore =
                  (resolvedModuleCore resolvedUnitModule)
                    { coreModuleDeclaredExports =
                        Just
                          ( DeclaredModuleExports
                              (SourceSpan 1 1)
                              [ModuleExportSelector (Just ValueNamespace) "missing"]
                          )
                    }
              }
          leadingStatement =
            withExpression
              (EBlock [SLet "ignored" (SourceSpan 1 1) (ETuple []), SExpr (SourceSpan 2 1) (ETuple [])])
              resolvedUnitModule
          inputFailure = [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreImportedInputsUnsupported TypedCoreNoFailureDetail]
          rootExpressionFailure = [TypedCoreProductionFailure (TypedCoreProductionExpressionPath ["App", "Main"] 0 []) TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
      assertUnsupported pathMismatch [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreModulePathMismatch TypedCoreNoFailureDetail]
      assertUnsupported importedValue inputFailure
      assertUnsupported importedData inputFailure
      assertUnsupported importedCapabilities inputFailure
      assertUnsupportedResolved unitFixture unsupportedRoot rootExpressionFailure
      assertUnsupportedResolved
        unitFixture
        nonLocalCall
        [TypedCoreProductionFailure (TypedCoreProductionExpressionPath ["App", "Main"] 0 []) TypedCoreNonLocalCallUnsupported (TypedCoreNameDetail "__kernel_toInt8")]
      assertUnsupportedResolved
        unitFixture
        unsupportedExport
        [TypedCoreProductionFailure (TypedCoreProductionModulePath ["App", "Main"]) TypedCoreUnsupportedExport (TypedCoreNameDetail "missing")]
      leadingStatementResult <- produceResolvedFixture unitFixture leadingStatement
      case typedCoreProductionStatus leadingStatementResult of
        TypedCoreProductionSucceeded programValue ->
          assertEqual "unit scalar binding typed-core validation" [] (validateTypedProgram programValue)
        _ -> failTest "unit scalar binding did not produce typed core"
    [] -> failTest "unit fixture is missing"

assertUnsupportedResolved :: Fixture -> ResolvedModule -> [TypedCoreProductionFailure] -> IO ()
assertUnsupportedResolved fixture resolvedModule expectedFailures = do
  result <- produceResolvedFixture fixture resolvedModule
  assertEqual
    (fixtureName fixture <> " production status")
    (TypedCoreProductionUnsupported expectedFailures)
    (typedCoreProductionStatus result)

withExpression :: Expr -> ResolvedModule -> ResolvedModule
withExpression expression moduleValue =
  moduleValue
    { resolvedModuleCore =
        CoreModule
          (Just ["App", "Main"])
          Nothing
          []
          expression
    }
