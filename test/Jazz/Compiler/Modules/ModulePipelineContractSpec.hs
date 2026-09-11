{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (toList)
import qualified Data.Foldable as Foldable
import Data.Functor.Identity (runIdentity)
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (..),
    CoreNodeId (..),
    CorePhase (Analyzed, Resolved),
    CoreSort (ExpressionSort, PatternSort, StatementSort),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Pattern (..),
    Statement (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol (BuiltinToInt8),
  )
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts (..), ResolvedReference (..), emptyResolvedNodeFacts)
import Jazz.Compiler.Diagnostics (Diagnostic, SourceSpan (..))
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Driver
  ( CompileResult,
    ResolvedPrelude (..),
    RunExecution (..),
    RunResult,
    buildAnalyzedSourceProgram,
    compileErrors,
    compileModuleGraphWithPrelude,
    compileWarnings,
    runCompileErrors,
    runExecution,
    runExitStatus,
    runModuleGraphWithPrelude,
    runModuleGraphWithPreludeAndHost,
    runOutput,
    runRuntimeErrors,
    runRuntimeValue,
    runSourceWithPrelude,
    runSourceWithPreludeAndHost,
  )
import Jazz.Compiler.ModuleAnalysis
  ( analyzeModule,
    dependencyImportInterface,
    moduleEvidenceCandidates,
  )
import Jazz.Compiler.ModuleCompiler
  ( analyzeProgram,
    analyzedProgramErrors,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventory,
    exportInventoryEntries,
  )
import Jazz.Compiler.ModuleGraph
  ( AnalyzedModuleFacts (..),
    CoreModule,
    CoreProgram,
    ModuleImport (..),
    PreludeArtifact (..),
    ResolvedModuleFacts (..),
    analyzedModuleDiagnostics,
    coreModuleBodyNode,
    coreModuleExpr,
    coreModuleFacts,
    coreModuleImports,
    coreModulePath,
    coreModuleStatements,
    coreProgramModules,
    coreProgramPrelude,
    lookupCoreModule,
  )
import Jazz.Compiler.ModuleIdentity
  ( ModulePath,
    SourceUnitOwner (..),
    mkModulePath,
    mkSourceFile,
    moduleIdentity,
    preludeModulePath,
    standaloneModulePath,
  )
import Jazz.Compiler.ModuleInterface
  ( ModuleInterface (..),
    ModuleValueBinding (..),
    emptyCompileInputs,
  )
import Jazz.Compiler.ModuleResolver (ModuleResolutionConfig (..), resolveProgramWithAmbientExports)
import Jazz.Compiler.ModuleRuntime
  ( RuntimeExport (..),
    RuntimeModule (runtimeModuleExports, runtimeModulePath),
    RuntimeProgram (runtimeProgramModules, runtimeProgramOutput),
    evaluateAnalyzedProgram,
    interpretAnalyzedProgram,
    lookupRuntimeModule,
  )
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (CapabilityNamespace, ConstructorNamespace, TypeNamespace, ValueNamespace),
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    identifierText,
    mkIdentifier,
    operatorBindingName,
  )
import Jazz.Compiler.Runtime
  ( RuntimeCell,
    evaluateRuntimeExpr,
    renderRuntimeValue,
    runtimeExprRequiresHost,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeObservationRequest (RuntimeObservationDisabled),
    RuntimeObservationResult (runtimeObservationOutcome),
  )
import Jazz.Compiler.Runtime.Outcome (runtimeOutcomeAsDiagnosticResult)
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost (..),
    RuntimeHostExit (..),
    disabledRuntimeHost,
    productionRuntimeHost,
  )
import Jazz.Compiler.SemanticFacts
  ( AnalyzedMethodSignature (..),
    AnalyzedNumericConstraint (..),
    AnalyzedPrimitiveConstraint (..),
    AnalyzedScheme (..),
    AnalyzedType,
    BinaryOperandTyping (..),
    BinaryOperation (..),
    CapabilityId (..),
    CoreBinderId (..),
    EvidenceReference (..),
    ExpressionFacts (..),
    ImplId (..),
    InstantiationTarget (..),
    MethodId (..),
    PatternConstructorFact (..),
    PatternFacts (..),
    PatternRefutability (..),
    RuntimeObligation (..),
    RuntimePlan (..),
    SemanticFactInvariantFailure (..),
    SemanticInstantiation (..),
    StatementDeclarationFact (..),
    StatementFacts (..),
  )
import Jazz.Compiler.SourceProgram (parseAndLowerStandaloneSource)
import Jazz.Compiler.TypeInference.Analyzed (attachAnalyzedExpression, projectAnalyzedMethodSignature)
import Jazz.Compiler.TypeInference.Result (inferredDiagnostics)
import Jazz.Compiler.TypeInference.Solver (freshIntegerLiteralType)
import Jazz.Compiler.TypeInference.State
  ( ExplicitInstantiationSeed (..),
    ExplicitInstantiationTarget (..),
    ExpressionEvidenceSeed (..),
    initialInferState,
    recordExplicitInstantiationSeed,
    recordExpressionEvidenceSeed,
    recordExpressionFactType,
    recordPatternFactSeed,
    recordStatementFactSeed,
  )
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    InferenceVariable (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    SchemePrimitiveConstraint (..),
    SemanticType (..),
    TypeBinding (..),
    TypeScheme (..),
    emptyScopeCapabilityFacts,
    quantifiedVariablesFromPreferred,
  )
import Jazz.Compiler.TypeRepresentation
  ( NumericType (NumericFloat64, NumericInt8),
    SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "ModulePipelineContract" tests

tests :: [NamedTest]
tests =
  [ ("standalone source and prelude keep separate graph identities", testStandaloneProgramOwnership),
    ("standalone prelude expressions retain effects and terminal values", testStandalonePreludeExecution),
    ("single-module analysis consumes complete imported interfaces", testSingleModuleAnalysis),
    ("runtime consumes analyzed declarations after source types are erased", testRuntimeUsesAnalyzedDeclarations),
    ("operation facts retain the numeric rule decision across equivalent aliases", testBinaryOperandAliasSelection),
    ("analyzed operations retain operand typing and alias selection", testAnalyzedBinaryOperations),
    ("analyzed expressions preserve literal-range constraints for backend specialization", testAnalyzedLiteralRangeFacts),
    ("successful inference attaches complete analyzed facts", testAnalyzedProgramFactsAreComplete),
    ("analyzed methods identify used and unused class parameters", testAnalyzedMethodParameterIdentity),
    ("method projection rejects variables outside the class binder", testAnalyzedMethodParameterBoundary),
    ("analyzed fact attachment rejects missing and duplicate entries", testAnalyzedFactInvariantFailures),
    ("dependency expressions are checked but not executed", testDependencyExpressionContract),
    ("analyzed interfaces expose only declared exports", testAnalyzedInterfacesExposeOnlyDeclaredExports),
    ("runtime modules publish only declared exports", testRuntimeModulePublishesDeclaredExports),
    ("analyzed modules retain private interfaces with public inventories", testAnalyzedModuleKeepsPrivateInterfaceWithPublicInventory),
    ("runtime modules publish explicit value exports only", testRuntimeModulePublishesExplicitExportsOnly),
    ("runtime modules publish methods only for public classes", testRuntimeModulePublishesPublicClassMethodsOnly),
    ("module export identities distinguish shadowed values and constructors", testModuleExportIdentityPreservesNamespaces),
    ("namespace-aware runtime exports publish selected value only", testNamespaceAwareRuntimeExportPublishesValueOnly),
    ("namespace-aware runtime exports publish selected constructor only", testNamespaceAwareRuntimeExportPublishesConstructorOnly),
    ("grouped exports publish selected constructors through interface and runtime inventories", testGroupedExportsPublishSelectedConstructor),
    ("analyzed generic constructor fields remain module-stable", testAnalyzedGenericConstructorFieldsRemainModuleStable),
    ("analyzed dependency terminal expressions are skipped", testAnalyzedDependencyTerminalExpressionIsSkipped),
    ("host-free and host-capable module paths preserve observable results", testModuleRuntimePathParity),
    ("run result projections distinguish all execution states", testRunResultProjectionInvariants),
    ("module graph execution carries one host through dependency exports", testModuleGraphInjectsRuntimeHost),
    ("alias imports stay qualified", testAliasIsolationContract),
    ("transitive imports do not leak", testTransitiveVisibilityContract),
    ("module diagnostics retain source paths", testSourcePathContract),
    ("lexical binders shadow imported and builtin names", testLexicalBindersShadowImportedAndBuiltinNames),
    ("explicit instantiations retain lexical binder identities through shadowing", testExplicitInstantiationBinderShadowing),
    ("explicit operator instantiations retain their binder identity", testExplicitOperatorInstantiationBinder),
    ("statement schemes are captured at each definition site", testStatementSchemesAreDefinitionSiteFacts),
    ("builtin aliases retain complete statement schemes", testBuiltinAliasStatementScheme),
    ("signed builtin aliases retain their authored schemes", testSignedBuiltinAliasStatementScheme)
  ]

-- Resolve real modules once, then check the entry from the public dependency
-- interface alone. This covers nominal data, explicit binder instantiation and
-- selected implementation evidence without giving the operation a program graph.
testStandaloneProgramOwnership :: IO ()
testStandaloneProgramOwnership = do
  source <- either (fail . show) pure (parseAndLowerStandaloneSource "saved = make. saved.")
  result <- buildAnalyzedSourceProgram defaultWarningSettings (PreludeExplicit "data Token = Token. make = Token.") source
  case result of
    Right (resolved, [], Just analyzed) -> do
      let entry = NonEmpty.last (coreProgramModules resolved)
      assertEqual "standalone root owner" (StandaloneSourceUnit standaloneModulePath) (resolvedNodeOwner (coreNodeFacts (coreModuleBodyNode entry)))
      case preludeModule (coreProgramPrelude resolved) of
        Just prelude -> do
          assertEqual "prelude root owner" (PreludeSourceUnit preludeModulePath) (resolvedNodeOwner (coreNodeFacts (coreModuleBodyNode prelude)))
          assertEqual "independent root node spaces" (coreNodeId (coreModuleBodyNode prelude)) (coreNodeId (coreModuleBodyNode entry))
          case ([node | SLet node _ _ <- coreModuleStatements prelude], coreModuleStatements entry) of
            ([definition], SLet _ _ (EVar use _) : _) ->
              assertEqual "source selects the prelude declaration" (LexicalReference <$> resolvedNodeBinder (coreNodeFacts definition)) (resolvedNodeReference (coreNodeFacts use))
            _ -> fail "unexpected standalone identity fixture shape"
        Nothing -> fail "missing independent prelude artifact"
      runtime <- either (fail . show) pure (evaluateAnalyzedProgram analyzed)
      assertEqual "standalone program result" (Just "Token") (renderRuntimeValue <$> runtimeProgramOutput runtime)
    other -> fail ("standalone graph analysis failed: " <> show other)

testStandalonePreludeExecution :: IO ()
testStandalonePreludeExecution = do
  calls <- newIORef []
  result <-
    runSourceWithPreludeAndHost
      (recordingHost calls)
      defaultWarningSettings
      (Just "__kernel_writeStdoutRaw! \"prelude\".")
      "__kernel_writeStdoutRaw! \"source\". 7."
  assertEqual "prelude/source compilation" [] (runCompileErrors result)
  assertEqual "prelude/source execution" [] (runRuntimeErrors result)
  assertEqual "prelude then source effects" ["prelude", "source"] =<< readIORef calls
  assertEqual "source terminal value" (Just "7") (runOutput result)
  emptyResult <- runSourceWithPrelude defaultWarningSettings (Just "42.") ""
  assertEqual "empty source retains prelude terminal value" (Just "42") (runOutput emptyResult)
  bindingResult <- runSourceWithPrelude defaultWarningSettings (Just "42.") "value = 1."
  assertEqual "source declaration clears prelude terminal value" Nothing (runOutput bindingResult)

testSingleModuleAnalysis :: IO ()
testSingleModuleAnalysis = do
  (resolved, analyzed) <- analyzeFixtureProgram factCompletenessSources
  let inputs = emptyCompileInputs defaultWarningSettings
      entryPath = nominalModulePath ("App" :| ["Main"])
  entry <- maybe (fail "missing resolved entry") pure (lookupCoreModule entryPath resolved)
  expected <- maybe (fail "missing analyzed entry") pure (lookupCoreModule entryPath analyzed)
  imports <- traverse (dependencyInterface resolved analyzed) (coreModuleImports entry)
  (inference, actual) <- analyzeModule inputs NamedSourceUnit False (mconcat imports) entry
  assertEqual "single-module diagnostics" [] (inferredDiagnostics inference)
  assertEqual "single-module facts, binders and evidence match program analysis" (Just expected) actual
  failing <- resolveFixtureProgram (Map.singleton "src/App/Main.jz" "module App::Main { 1 True. }")
  let failingEntry = NonEmpty.head (coreProgramModules failing)
  (programDiagnostics, _) <- analyzeProgram inputs failing
  (failedInference, failedModule) <- analyzeModule inputs NamedSourceUnit False mempty failingEntry
  assertEqual "failed module has no analyzed artifact" Nothing failedModule
  assertEqual "single-module diagnostic order matches program analysis" programDiagnostics (inferredDiagnostics failedInference)
  where
    dependencyInterface resolved analyzed importDecl = do
      let path = importedModule importDecl
      dependency <- maybe (fail "missing resolved dependency") pure (lookupCoreModule path resolved)
      checked <- maybe (fail "missing analyzed dependency") pure (lookupCoreModule path analyzed)
      pure $
        dependencyImportInterface
          importDecl
          ( resolvedModuleExports (coreModuleFacts dependency),
            analyzedModuleInterface (coreModuleFacts checked),
            moduleEvidenceCandidates dependency
          )

testBinaryOperandAliasSelection :: IO ()
testBinaryOperandAliasSelection = do
  (_, analyzed) <-
    analyzeFixtureProgram
      ( Map.singleton
          "src/App/Main.jz"
          "module App::Main { a :: Float. a = 1.0. b :: Float64. b = 2.0. a + b. }"
      )
  coreModule <-
    maybe
      (fail "missing analyzed module")
      pure
      (lookupCoreModule (nominalModulePath ("App" :| ["Main"])) analyzed)
  case coreModuleExpr coreModule of
    EBlock _ statements -> case [coreNodeFacts node | SExpr _ (EBinary node _ _ _) <- statements] of
      [facts] -> do
        assertEqual
          "numeric rule selects the concrete alias"
          (SemanticNumeric NumericFloat64)
          (expressionSemanticType facts)
        assertEqual
          "operand fact preserves that exact decision"
          (Just (UniformBinaryOperands (SemanticNumeric NumericFloat64)))
          (binaryOperationOperandTyping <$> expressionBinaryOperation facts)
      _ -> fail "missing binary operation"
    _ -> fail "expected analyzed block"

testAnalyzedBinaryOperations :: IO ()
testAnalyzedBinaryOperations = do
  (_, analyzed) <-
    analyzeFixtureProgram
      ( Map.singleton
          "src/App/Main.jz"
          "module App::Main { add = (+). x :: Int8. x = 1. x == 2. add x 3. 1 + 2.0. }"
      )
  coreModule <-
    maybe
      (fail "missing analyzed module")
      pure
      (lookupCoreModule (nominalModulePath ("App" :| ["Main"])) analyzed)
  case coreModuleExpr coreModule of
    EBlock _ statements ->
      case [expression | SExpr _ expression <- statements] of
        [ EBinary comparisonNode _ (EVar leftNode _) (ELit rightNode _),
          EApply applicationNode (EApply _ _ (EVar aliasLeftNode _)) (ELit aliasRightNode _),
          EBinary promotionNode _ (ELit promotionLeftNode _) (ELit promotionRightNode _)
          ] -> do
            assertEqual
              "comparison result remains Bool"
              SemanticBool
              (expressionSemanticType (coreNodeFacts comparisonNode))
            assertEqual
              "comparison operands use the signed context"
              ( Just
                  ( BinaryOperation
                      "=="
                      (UniformBinaryOperands (SemanticNumeric NumericInt8))
                      (coreNodeId leftNode)
                      (coreNodeId rightNode)
                  )
              )
              (expressionBinaryOperation (coreNodeFacts comparisonNode))
            assertEqual
              "operator alias retains its selected primitive and original operands"
              ( Just
                  ( BinaryOperation
                      "+"
                      (UniformBinaryOperands (SemanticNumeric NumericInt8))
                      (coreNodeId aliasLeftNode)
                      (coreNodeId aliasRightNode)
                  )
              )
              (expressionBinaryOperation (coreNodeFacts applicationNode))
            assertEqual
              "implicit promotion is distinct from uniform operand typing"
              ( Just
                  ( BinaryOperation
                      "+"
                      Float64PromotedOperands
                      (coreNodeId promotionLeftNode)
                      (coreNodeId promotionRightNode)
                  )
              )
              (expressionBinaryOperation (coreNodeFacts promotionNode))
        expressions -> fail ("unexpected operation fixture: " <> show expressions)
    expression -> fail ("expected analyzed block: " <> show expression)

testAnalyzedLiteralRangeFacts :: IO ()
testAnalyzedLiteralRangeFacts = do
  let nodeId = CoreNodeId 17
      expression = ELit (CoreNode nodeId (SourceSpan 1 1) (emptyResolvedNodeFacts (NamedSourceUnit (nominalModulePath ("App" :| ["Main"]))))) (LInt 255)
      (literalType, literalState) = freshIntegerLiteralType (IntegerLiteralRange 0 255) initialInferState
      state = recordExpressionFactType nodeId literalType literalState
  case attachAnalyzedExpression state expression of
    Right (ELit (CoreNode _ _ facts) _) -> do
      assertEqual "uncommitted numeric representation" literalType (expressionSemanticType facts)
      assertEqual
        "backend retains the solver's complete literal range"
        [AnalyzedIntegralLiteralNumericConstraint 0 255]
        (Map.elems (expressionNumericConstraints facts))
    result -> fail ("literal fact attachment failed: " <> show result)

testAnalyzedProgramFactsAreComplete :: IO ()
testAnalyzedProgramFactsAreComplete = do
  resolved <- resolveFixtureProgram factCompletenessSources
  (diagnostics, maybeAnalyzed) <-
    analyzeProgram
      (emptyCompileInputs defaultWarningSettings)
      resolved
  assertEqual "analyzed diagnostics" [] diagnostics
  case maybeAnalyzed of
    Nothing -> fail "successful inference did not produce analyzed core"
    Just analyzed -> assertAnalyzedProgramFacts resolved analyzed

  failingResolved <- resolveFixtureProgram (Map.singleton "src/App/Main.jz" "module App::Main { 1 True. }")
  (failingDiagnostics, failingAnalyzed) <-
    analyzeProgram (emptyCompileInputs defaultWarningSettings) failingResolved
  assertEqual "failed inference produces no analyzed core" Nothing failingAnalyzed
  assertEqual "failed inference emits one or more diagnostics" False (null failingDiagnostics)

assertAnalyzedProgramFacts :: CoreProgram 'Resolved -> CoreProgram 'Analyzed -> IO ()
assertAnalyzedProgramFacts resolvedProgram analyzedProgram = do
  mapM_ assertModule (NonEmpty.toList (coreProgramModules resolvedProgram))
  case foldMap (expressionEvidenceInventory . coreModuleExpr) (coreProgramModules analyzedProgram) of
    [evidence] -> do
      assertEqual "capability evidence target" SemanticInt (evidenceType evidence)
      assertEqual
        "capability evidence preserves the selected canonical identities"
        (expectedEvidenceIdentities resolvedProgram)
        [(evidenceCapability evidence, evidenceImplementation evidence, evidenceMethod evidence)]
    evidence -> fail ("expected exactly one selected capability evidence fact, got " <> show evidence)
  let analyzedBinders = foldMap moduleBinderIds (coreProgramModules analyzedProgram)
      instantiations = foldMap (expressionInstantiationInventory . coreModuleExpr) (coreProgramModules analyzedProgram)
      analyzedSchemes = foldMap moduleSchemes (coreProgramModules analyzedProgram)
  case instantiations of
    [SemanticInstantiation (LexicalInstantiation binder) (SemanticInt :| [])] ->
      assertEqual "explicit instantiation references an analyzed declaration binder" True (binder `elem` analyzedBinders)
    values -> fail ("expected one exact Int instantiation, got " <> show values)
  assertEqual
    "generalized schemes preserve quantified variables"
    True
    (any (not . null . analyzedSchemeVariables) analyzedSchemes)
  where
    assertModule resolvedModule =
      case lookupCoreModule (coreModulePath resolvedModule) analyzedProgram of
        Nothing -> fail "analyzed program lost a resolved module"
        Just analyzedModule -> do
          assertEqual
            "node ids and source spans"
            (moduleNodeIdentities resolvedModule)
            (moduleNodeIdentities analyzedModule)
          assertEqual
            "authored export selector ordering"
            (resolvedModuleExportSelectors (coreModuleFacts resolvedModule))
            (analyzedModuleExportSelectors (coreModuleFacts analyzedModule))
          assertEqual
            "analyzed export inventory"
            (resolvedModuleExports (coreModuleFacts resolvedModule))
            (analyzedModuleExports (coreModuleFacts analyzedModule))
          assertEqual
            "analyzed module diagnostics"
            []
            (analyzedModuleDiagnostics (coreModuleFacts analyzedModule))
          assertExprFacts (coreModuleExpr analyzedModule)
          mapM_ assertImportFacts (coreModuleImports analyzedModule)

    assertImportFacts importDecl =
      case moduleImportNode importDecl of
        CoreNode _ _ facts ->
          case statementDeclarationFact facts of
            ImportDeclaration _ -> pure ()
            declarationFact -> fail ("unexpected analyzed import declaration fact: " <> show declarationFact)

testAnalyzedMethodParameterIdentity :: IO ()
testAnalyzedMethodParameterIdentity = do
  (_, analyzed) <-
    analyzeFixtureProgram
      ( Map.singleton
          "src/App/Main.jz"
          "module App::Main { class Probe(a) { nested :: [a] -> [a]. constant :: Int -> Bool. }. 0. }"
      )
  let methods =
        Map.fromList
          [ (identifierText name, signature)
          | SClass _ _ _ declarations <- coreModuleStatements (NonEmpty.head (coreProgramModules analyzed)),
            ClassMethodSignature node name _ <- declarations,
            MethodDeclaration _ signature <- [statementDeclarationFact (coreNodeFacts node)]
          ]
  case (Map.lookup "nested" methods, Map.lookup "constant" methods) of
    (Just nested, Just constant) -> do
      let parameter = SemanticVariable (analyzedMethodClassParameter nested)
      assertEqual
        "nested occurrences refer to the explicit class parameter"
        (SemanticFunction (SemanticList parameter) (SemanticList parameter))
        (analyzedMethodType nested)
      assertEqual
        "a method can leave its class parameter unused"
        (SemanticFunction SemanticInt SemanticBool)
        (analyzedMethodType constant)
    _ -> fail "missing analyzed Probe methods"

testAnalyzedMethodParameterBoundary :: IO ()
testAnalyzedMethodParameterBoundary =
  mapM_
    checkRejected
    [ TypeFunction foreignVariable foreignVariable,
      TypeFunction classVariable foreignVariable
    ]
  where
    classVariable = TypeVariable (BuiltinName (mkIdentifier "a"))
    foreignVariable = TypeVariable (BuiltinName (mkIdentifier "b"))
    checkRejected signatureType =
      assertEqual
        "an unexpected variable fails projection instead of dropping or guessing the binder"
        (Left (InvalidAnalyzedMethodSignature "Probe::bad"))
        (projectAnalyzedMethodSignature initialInferState "Probe::bad" (ClassMethodType "a" (SignatureType signatureType)))

testAnalyzedFactInvariantFailures :: IO ()
testAnalyzedFactInvariantFailures = do
  let expressionId = CoreNodeId 41
      expression = ELit (CoreNode expressionId (SourceSpan 1 1) (emptyResolvedNodeFacts (NamedSourceUnit modulePath))) (LInt 1)
      expressionOnce = recordExpressionFactType expressionId SemanticInt initialInferState
      expressionTwice = recordExpressionFactType expressionId SemanticInt expressionOnce
      modulePath = nominalModulePath ("Fact" :| [])
  assertEqual
    "missing expression fact"
    (Left (MissingExpressionFacts expressionId :| []))
    (attachAnalyzedExpression initialInferState expression)
  assertEqual
    "duplicate expression fact"
    (Left (DuplicateExpressionFacts expressionId :| []))
    (attachAnalyzedExpression expressionTwice expression)

  let leftId = CoreNodeId 42
      rightId = CoreNodeId 43
      pair =
        ETuple
          (CoreNode expressionId (SourceSpan 1 1) (emptyResolvedNodeFacts (NamedSourceUnit modulePath)))
          [ ELit (CoreNode leftId (SourceSpan 1 2) (emptyResolvedNodeFacts (NamedSourceUnit modulePath))) (LInt 1),
            ELit (CoreNode rightId (SourceSpan 1 3) (emptyResolvedNodeFacts (NamedSourceUnit modulePath))) (LInt 2)
          ]
  assertEqual
    "recorded failures precede independent missing child facts in source order"
    (Left (DuplicateExpressionFacts expressionId :| [MissingExpressionFacts leftId, MissingExpressionFacts rightId]))
    (attachAnalyzedExpression expressionTwice pair)

  let implementationId = ImplId (NamedSourceUnit modulePath, CoreNodeId 100)
      evidenceSeed =
        ExpressionEvidenceSeed
          { evidenceSeedCapability = CapabilityId (BuiltinName (mkIdentifier "Eq")),
            evidenceSeedImplementation = implementationId,
            evidenceSeedMethod = MethodId (implementationId, mkIdentifier "equals"),
            evidenceSeedType = SemanticInt
          }
      evidenceOnce = recordExpressionEvidenceSeed expressionId evidenceSeed expressionOnce
      evidenceTwice = recordExpressionEvidenceSeed expressionId evidenceSeed evidenceOnce
  assertEqual
    "duplicate expression evidence fact"
    (Left (DuplicateExpressionFacts expressionId :| []))
    (attachAnalyzedExpression evidenceTwice expression)

  let typeApplicationId = CoreNodeId 53
      typeApplicationFunctionId = CoreNodeId 54
      missingBinderName = BuiltinName (mkIdentifier "identity")
      mismatchedBinderName = BuiltinName (mkIdentifier "otherIdentity")
      lexicalBinderId = CoreBinderId (NamedSourceUnit modulePath, CoreNodeId 52)
      typeApplication =
        ETypeApplication
          (CoreNode typeApplicationId (SourceSpan 1 1) (emptyResolvedNodeFacts (NamedSourceUnit modulePath)))
          (EVar (CoreNode typeApplicationFunctionId (SourceSpan 1 1) ((emptyResolvedNodeFacts (NamedSourceUnit modulePath)) {resolvedNodeReference = Just (LexicalReference lexicalBinderId)})) missingBinderName)
          (SourceSpan 1 10)
          TypeInt
      typeApplicationState =
        recordExpressionFactType
          typeApplicationId
          SemanticBool
          (recordExpressionFactType typeApplicationFunctionId SemanticBool initialInferState)
      explicitSeed =
        ExplicitInstantiationSeed
          { explicitInstantiationSeedTarget = ExplicitBinderInstantiation missingBinderName,
            explicitInstantiationSeedArguments = SemanticBool :| []
          }
      seededTypeApplicationState =
        recordExplicitInstantiationSeed typeApplicationId explicitSeed typeApplicationState
  assertEqual
    "explicit type application requires a recorded inference decision"
    (Left (MissingExplicitInstantiationSeed typeApplicationId :| []))
    (attachAnalyzedExpression typeApplicationState typeApplication)
  case attachAnalyzedExpression
    seededTypeApplicationState
    typeApplication of
    Right (ETypeApplication (CoreNode _ _ facts) _ _ _) ->
      assertEqual
        "attachment trusts the final solver argument rather than reconstructing common source syntax"
        [SemanticInstantiation (LexicalInstantiation lexicalBinderId) (SemanticBool :| [])]
        (expressionInstantiations facts)
    result -> fail ("expected a seeded analyzed explicit type application, got " <> show result)
  assertEqual
    "explicit type application seed is unique per node"
    (Left (DuplicateExplicitInstantiationSeed typeApplicationId :| []))
    ( attachAnalyzedExpression
        (recordExplicitInstantiationSeed typeApplicationId explicitSeed seededTypeApplicationState)
        typeApplication
    )
  let mismatchedSeed =
        explicitSeed
          { explicitInstantiationSeedTarget = ExplicitBinderInstantiation mismatchedBinderName
          }
  assertEqual
    "explicit type application seed target matches the resolved expression"
    (Left (MismatchedExplicitInstantiationSeed typeApplicationId missingBinderName mismatchedBinderName :| []))
    ( attachAnalyzedExpression
        (recordExplicitInstantiationSeed typeApplicationId mismatchedSeed typeApplicationState)
        typeApplication
    )
  assertEqual
    "explicit type application still requires a lexical binder identity"
    (Left (MissingExplicitInstantiationBinder typeApplicationId missingBinderName :| []))
    ( attachAnalyzedExpression
        seededTypeApplicationState
        ( ETypeApplication
            (CoreNode typeApplicationId (SourceSpan 1 1) (emptyResolvedNodeFacts (NamedSourceUnit modulePath)))
            (EVar (CoreNode typeApplicationFunctionId (SourceSpan 1 1) (emptyResolvedNodeFacts (NamedSourceUnit modulePath))) missingBinderName)
            (SourceSpan 1 10)
            TypeInt
        )
    )

  let caseId = CoreNodeId 42
      scrutineeId = CoreNodeId 43
      armId = CoreNodeId 44
      patternId = CoreNodeId 45
      bodyId = CoreNodeId 46
      patternValue = PWildcard (CoreNode patternId (SourceSpan 1 5) (emptyResolvedNodeFacts (NamedSourceUnit modulePath)))
      caseExpression =
        EPatternCase
          (CoreNode caseId (SourceSpan 1 1) (emptyResolvedNodeFacts (NamedSourceUnit modulePath)))
          (ELit (CoreNode scrutineeId (SourceSpan 1 3) (emptyResolvedNodeFacts (NamedSourceUnit modulePath))) (LInt 1))
          [CaseArm (CoreNode armId (SourceSpan 1 5) (emptyResolvedNodeFacts (NamedSourceUnit modulePath))) patternValue Nothing (ELit (CoreNode bodyId (SourceSpan 1 10) (emptyResolvedNodeFacts (NamedSourceUnit modulePath))) (LInt 1))]
      expressionCompleteState =
        foldr
          (\nodeId -> recordExpressionFactType nodeId SemanticInt)
          initialInferState
          [caseId, scrutineeId, armId, bodyId]
      patternFacts = PatternFacts (emptyResolvedNodeFacts (NamedSourceUnit modulePath)) Map.empty PatternHasNoConstructor IrrefutablePattern
      patternOnce = recordPatternFactSeed patternId patternFacts expressionCompleteState
      patternTwice = recordPatternFactSeed patternId patternFacts patternOnce
  assertEqual
    "missing pattern fact"
    (Left (MissingPatternFacts patternId :| []))
    (attachAnalyzedExpression expressionCompleteState caseExpression)
  assertEqual
    "duplicate pattern fact"
    (Left (DuplicatePatternFacts patternId :| []))
    (attachAnalyzedExpression patternTwice caseExpression)

  let blockId = CoreNodeId 47
      statementId = CoreNodeId 48
      statementExpressionId = CoreNodeId 49
      blockExpression =
        EBlock
          (CoreNode blockId (SourceSpan 1 1) (emptyResolvedNodeFacts (NamedSourceUnit modulePath)))
          [SExpr (CoreNode statementId (SourceSpan 1 3) (emptyResolvedNodeFacts (NamedSourceUnit modulePath))) (ELit (CoreNode statementExpressionId (SourceSpan 1 3) (emptyResolvedNodeFacts (NamedSourceUnit modulePath))) (LInt 1))]
      blockExpressionState =
        recordExpressionFactType
          blockId
          SemanticInt
          (recordExpressionFactType statementExpressionId SemanticInt initialInferState)
      statementOnce = recordStatementFactSeed statementId ([], ExpressionDeclaration) blockExpressionState
      statementTwice = recordStatementFactSeed statementId ([], ExpressionDeclaration) statementOnce
  assertEqual
    "missing statement fact"
    (Left (MissingStatementFacts statementId :| []))
    (attachAnalyzedExpression blockExpressionState blockExpression)
  assertEqual
    "duplicate statement fact"
    (Left (DuplicateStatementFacts statementId :| []))
    (attachAnalyzedExpression statementTwice blockExpression)

  let aliasBlockId = CoreNodeId 55
      aliasStatementId = CoreNodeId 56
      aliasExpressionId = CoreNodeId 57
      aliasName = BuiltinName (mkIdentifier "alias")
      aliasExpression = EVar (CoreNode aliasExpressionId (SourceSpan 1 9) (emptyResolvedNodeFacts (NamedSourceUnit modulePath))) (BuiltinName (mkIdentifier "__kernel_toInt8"))
      aliasBlock =
        EBlock
          (CoreNode aliasBlockId (SourceSpan 1 1) (emptyResolvedNodeFacts (NamedSourceUnit modulePath)))
          [SLet (CoreNode aliasStatementId (SourceSpan 1 1) ((emptyResolvedNodeFacts (NamedSourceUnit modulePath)) {resolvedNodeBinder = Just (CoreBinderId (NamedSourceUnit modulePath, aliasStatementId))})) aliasName aliasExpression]
      aliasState =
        recordStatementFactSeed
          aliasStatementId
          ([(aliasName, BuiltinAliasTypeBinding BuiltinToInt8)], ValueDeclaration aliasName)
          ( recordExpressionFactType
              aliasBlockId
              (SemanticFunction SemanticInt (SemanticNumeric NumericInt8))
              ( recordExpressionFactType
                  aliasExpressionId
                  (SemanticFunction SemanticInt (SemanticNumeric NumericInt8))
                  initialInferState
              )
          )
  assertEqual
    "statement binders cannot silently drop an unprojected scheme"
    (Left (MissingStatementScheme aliasStatementId (CoreBinderId (NamedSourceUnit modulePath, aliasStatementId)) :| []))
    (attachAnalyzedExpression aliasState aliasBlock)

  let rangeBlockId = CoreNodeId 50
      rangeStatementId = CoreNodeId 51
      rangeExpressionId = CoreNodeId 52
      rangeName = BuiltinName (mkIdentifier "range")
      rangeVariable = InferenceVariable 0
      rangeScheme =
        TypeScheme
          { schemeQuantifiedVariables = quantifiedVariablesFromPreferred [rangeVariable] (Set.singleton rangeVariable),
            schemeClassConstraints = [],
            schemePrimitiveConstraints =
              [ TypeSchemeNumericConstraint
                  (IntegralLiteralNumericConstraint (IntegerLiteralRange 1 1))
                  (SemanticVariable rangeVariable)
              ],
            schemeDefiningCapabilities = emptyScopeCapabilityFacts,
            schemeResultType = SemanticVariable rangeVariable
          }
      rangeExpression = ELit (CoreNode rangeExpressionId (SourceSpan 1 9) (emptyResolvedNodeFacts (NamedSourceUnit modulePath))) (LInt 1)
      rangeBlock =
        EBlock
          (CoreNode rangeBlockId (SourceSpan 1 1) (emptyResolvedNodeFacts (NamedSourceUnit modulePath)))
          [SLet (CoreNode rangeStatementId (SourceSpan 1 1) ((emptyResolvedNodeFacts (NamedSourceUnit modulePath)) {resolvedNodeBinder = Just (CoreBinderId (NamedSourceUnit modulePath, rangeStatementId))})) rangeName rangeExpression]
      rangeState =
        recordStatementFactSeed
          rangeStatementId
          ([(rangeName, SchemeTypeBinding rangeScheme)], ValueDeclaration rangeName)
          ( recordExpressionFactType
              rangeBlockId
              (SemanticVariable rangeVariable)
              (recordExpressionFactType rangeExpressionId (SemanticVariable rangeVariable) initialInferState)
          )
  case attachAnalyzedExpression rangeState rangeBlock of
    Right (EBlock _ [SLet (CoreNode _ _ facts) _ _]) ->
      assertEqual
        "generalized schemes preserve integral literal ranges"
        True
        (any schemeHasLiteralRange (Map.elems (statementGeneralizedSchemes facts)))
    result -> fail ("failed to attach literal-range scheme facts: " <> show result)

type NodeIdentity = (CoreNodeId, SourceSpan)

moduleNodeIdentities :: CoreModule phase -> [NodeIdentity]
moduleNodeIdentities coreModule =
  exprNodeIdentities (coreModuleExpr coreModule)
    <> [nodeIdentity node | importDecl <- coreModuleImports coreModule, let node = moduleImportNode importDecl]

nodeIdentity :: CoreNode phase sort -> NodeIdentity
nodeIdentity node = (coreNodeId node, coreNodeSpan node)

exprNodeIdentities :: Expr phase -> [NodeIdentity]
exprNodeIdentities expression =
  nodeIdentity (exprNode expression)
    : case expression of
      ELambda _ _ body -> exprNodeIdentities body
      EList _ values -> foldMap exprNodeIdentities values
      ETuple _ values -> foldMap exprNodeIdentities values
      EApply _ function argument -> exprNodeIdentities function <> exprNodeIdentities argument
      ETypeApplication _ function _ _ -> exprNodeIdentities function
      EIf _ condition whenTrue whenFalse -> foldMap exprNodeIdentities [condition, whenTrue, whenFalse]
      EPatternCase _ scrutinee arms -> exprNodeIdentities scrutinee <> foldMap caseArmNodeIdentities arms
      EBinary _ _ left right -> exprNodeIdentities left <> exprNodeIdentities right
      ESectionLeft _ left _ -> exprNodeIdentities left
      ESectionRight _ _ right -> exprNodeIdentities right
      EBlock _ statements -> foldMap statementNodeIdentities statements
      _ -> []

caseArmNodeIdentities :: CaseArm phase -> [NodeIdentity]
caseArmNodeIdentities (CaseArm node patternValue guard body) =
  nodeIdentity node
    : patternNodeIdentities patternValue
      <> foldMap exprNodeIdentities guard
      <> exprNodeIdentities body

patternNodeIdentities :: Pattern phase -> [NodeIdentity]
patternNodeIdentities patternValue =
  nodeIdentity (patternNode patternValue)
    : case patternValue of
      PConstructor _ _ patterns -> foldMap patternNodeIdentities patterns
      PList _ patterns -> foldMap patternNodeIdentities patterns
      PConsList _ headPattern tailPattern -> patternNodeIdentities headPattern <> patternNodeIdentities tailPattern
      PTuple _ patterns -> foldMap patternNodeIdentities patterns
      PAs _ _ nested -> patternNodeIdentities nested
      POr _ alternatives -> foldMap patternNodeIdentities alternatives
      _ -> []

statementNodeIdentities :: Statement phase -> [NodeIdentity]
statementNodeIdentities statement =
  nodeIdentity (statementCoreNode statement)
    : case statement of
      SLet _ _ value -> exprNodeIdentities value
      SData _ _ _ constructors ->
        [nodeIdentity node | DataConstructor node _ _ <- constructors]
      SClass _ _ _ methods ->
        [nodeIdentity node | ClassMethodSignature node _ _ <- methods]
      SImpl _ _ _ methods ->
        foldMap (\(ImplMethod node _ body) -> nodeIdentity node : exprNodeIdentities body) methods
      SExpr _ value -> exprNodeIdentities value
      _ -> []

assertExprFacts :: Expr 'Analyzed -> IO ()
assertExprFacts expression = do
  assertExpressionNodeFacts (exprNode expression)
  case expression of
    ELit (CoreNode _ _ facts) (LInt _)
      | SemanticNumeric target <- expressionSemanticType facts ->
          let RuntimePlan obligations = expressionRuntimePlan facts
           in assertEqual
                "integer literal runtime plan specializes its representation"
                True
                (SpecializeNumericLiteral target `elem` obligations)
    _ -> pure ()
  case expression of
    ELambda _ _ body -> assertExprFacts body
    EList _ values -> mapM_ assertExprFacts values
    ETuple _ values -> mapM_ assertExprFacts values
    EApply _ function argument -> mapM_ assertExprFacts [function, argument]
    ETypeApplication node function _ _ -> do
      case node of
        CoreNode _ _ facts -> assertEqual "explicit type application has one instantiation" 1 (length (expressionInstantiations facts))
      assertExprFacts function
    EIf _ condition whenTrue whenFalse -> mapM_ assertExprFacts [condition, whenTrue, whenFalse]
    EPatternCase _ scrutinee arms -> assertExprFacts scrutinee >> mapM_ assertCaseArmFacts arms
    EBinary _ _ left right -> mapM_ assertExprFacts [left, right]
    ESectionLeft _ left _ -> assertExprFacts left
    ESectionRight _ _ right -> assertExprFacts right
    EBlock _ statements -> mapM_ assertStatementFacts statements
    _ -> pure ()

assertExpressionNodeFacts :: CoreNode 'Analyzed 'ExpressionSort -> IO ()
assertExpressionNodeFacts (CoreNode _ _ facts) = do
  case reverse (toList obligations) of
    ConstrainResult resultType : _ ->
      assertEqual "runtime result constraint is concrete" True (Foldable.null resultType)
    _ -> pure ()
  case NonEmpty.nonEmpty (expressionEvidence facts) of
    Nothing -> pure ()
    Just evidence -> assertEqual "runtime plan supplies selected evidence" True (SupplyEvidence evidence `elem` obligations)
  where
    RuntimePlan obligations = expressionRuntimePlan facts

expressionEvidenceInventory :: Expr 'Analyzed -> [EvidenceReference]
expressionEvidenceInventory expression =
  expressionNodeEvidence expression
    <> case expression of
      ELambda _ _ body -> expressionEvidenceInventory body
      EList _ values -> foldMap expressionEvidenceInventory values
      ETuple _ values -> foldMap expressionEvidenceInventory values
      EApply _ function argument -> expressionEvidenceInventory function <> expressionEvidenceInventory argument
      ETypeApplication _ function _ _ -> expressionEvidenceInventory function
      EIf _ condition whenTrue whenFalse -> foldMap expressionEvidenceInventory [condition, whenTrue, whenFalse]
      EPatternCase _ scrutinee arms -> expressionEvidenceInventory scrutinee <> foldMap armEvidence arms
      EBinary _ _ left right -> expressionEvidenceInventory left <> expressionEvidenceInventory right
      ESectionLeft _ left _ -> expressionEvidenceInventory left
      ESectionRight _ _ right -> expressionEvidenceInventory right
      EBlock _ statements -> foldMap statementEvidence statements
      _ -> []
  where
    expressionNodeEvidence value =
      case exprNode value of
        CoreNode _ _ facts -> expressionEvidence facts
    armEvidence (CaseArm (CoreNode _ _ facts) _ guard body) =
      expressionEvidence facts <> foldMap expressionEvidenceInventory guard <> expressionEvidenceInventory body
    statementEvidence statement =
      case statement of
        SLet _ _ value -> expressionEvidenceInventory value
        SImpl _ _ _ methods -> foldMap (\(ImplMethod _ _ body) -> expressionEvidenceInventory body) methods
        SExpr _ value -> expressionEvidenceInventory value
        _ -> []

expressionInstantiationInventory :: Expr 'Analyzed -> [SemanticInstantiation]
expressionInstantiationInventory expression =
  expressionNodeInstantiations expression
    <> case expression of
      ELambda _ _ body -> expressionInstantiationInventory body
      EList _ values -> foldMap expressionInstantiationInventory values
      ETuple _ values -> foldMap expressionInstantiationInventory values
      EApply _ function argument -> expressionInstantiationInventory function <> expressionInstantiationInventory argument
      ETypeApplication _ function _ _ -> expressionInstantiationInventory function
      EIf _ condition whenTrue whenFalse -> foldMap expressionInstantiationInventory [condition, whenTrue, whenFalse]
      EPatternCase _ scrutinee arms -> expressionInstantiationInventory scrutinee <> foldMap armInstantiations arms
      EBinary _ _ left right -> expressionInstantiationInventory left <> expressionInstantiationInventory right
      ESectionLeft _ left _ -> expressionInstantiationInventory left
      ESectionRight _ _ right -> expressionInstantiationInventory right
      EBlock _ statements -> foldMap statementInstantiations statements
      _ -> []
  where
    expressionNodeInstantiations value =
      case exprNode value of
        CoreNode _ _ facts -> expressionInstantiations facts
    armInstantiations (CaseArm (CoreNode _ _ facts) _ guard body) =
      expressionInstantiations facts <> foldMap expressionInstantiationInventory guard <> expressionInstantiationInventory body
    statementInstantiations statement =
      case statement of
        SLet _ _ value -> expressionInstantiationInventory value
        SImpl _ _ _ methods -> foldMap (\(ImplMethod _ _ body) -> expressionInstantiationInventory body) methods
        SExpr _ value -> expressionInstantiationInventory value
        _ -> []

moduleBinderIds :: CoreModule 'Analyzed -> [CoreBinderId]
moduleBinderIds = foldMap statementBinderInventory . moduleStatements
  where
    moduleStatements coreModule =
      case coreModuleExpr coreModule of
        EBlock _ statements -> statements
        _ -> []
    statementBinderInventory statement =
      nodeBinders (statementCoreNode statement)
        <> case statement of
          SData _ _ _ constructors -> foldMap (\(DataConstructor node _ _) -> nodeBinders node) constructors
          SClass _ _ _ methods -> foldMap (\(ClassMethodSignature node _ _) -> nodeBinders node) methods
          SImpl _ _ _ methods -> foldMap (\(ImplMethod node _ _) -> nodeBinders node) methods
          _ -> []
    nodeBinders (CoreNode _ _ facts) = statementBinderIds facts

moduleSchemes :: CoreModule 'Analyzed -> [AnalyzedScheme]
moduleSchemes = foldMap statementSchemes . moduleStatements
  where
    moduleStatements coreModule =
      case coreModuleExpr coreModule of
        EBlock _ statements -> statements
        _ -> []
    statementSchemes statement =
      nodeSchemes (statementCoreNode statement)
        <> case statement of
          SData _ _ _ constructors -> foldMap (\(DataConstructor node _ _) -> nodeSchemes node) constructors
          SClass _ _ _ methods -> foldMap (\(ClassMethodSignature node _ _) -> nodeSchemes node) methods
          SImpl _ _ _ methods -> foldMap (\(ImplMethod node _ _) -> nodeSchemes node) methods
          _ -> []
    nodeSchemes (CoreNode _ _ facts) = Map.elems (statementGeneralizedSchemes facts)

schemeHasLiteralRange :: AnalyzedScheme -> Bool
schemeHasLiteralRange scheme =
  any isLiteralRange (analyzedSchemePrimitiveConstraints scheme)
  where
    isLiteralRange constraint =
      case constraint of
        AnalyzedNumericPrimitiveConstraint (AnalyzedIntegralLiteralNumericConstraint 1 1) _ -> True
        _ -> False

expectedEvidenceIdentities :: CoreProgram 'Resolved -> [(CapabilityId, ImplId, Maybe MethodId)]
expectedEvidenceIdentities program =
  [ ( CapabilityId
        ( UserName
            ( ResolvedUserName
                (ImportedModule (coreModulePath coreModule))
                CapabilityNamespace
                (mkIdentifier (identifierText capabilityName))
            )
        ),
      implementationId,
      Just (MethodId (implementationId, mkIdentifier (identifierText methodName)))
    )
  | coreModule <- NonEmpty.toList (coreProgramModules program),
    statement <- moduleStatements coreModule,
    SImpl implementationNode capabilityName [_] methods <- [statement],
    let implementationId = ImplId (NamedSourceUnit (coreModulePath coreModule), coreNodeId implementationNode),
    ImplMethod _ methodName _ <- methods,
    identifierText methodName == "equals"
  ]
  where
    moduleStatements coreModule =
      case coreModuleExpr coreModule of
        EBlock _ statements -> statements
        _ -> []

assertCaseArmFacts :: CaseArm 'Analyzed -> IO ()
assertCaseArmFacts (CaseArm node patternValue guard body) = do
  assertExpressionNodeFacts node
  assertPatternFacts patternValue
  mapM_ assertExprFacts guard
  assertExprFacts body

assertPatternFacts :: Pattern 'Analyzed -> IO ()
assertPatternFacts patternValue = do
  case patternNode patternValue of
    CoreNode _ _ facts -> do
      assertEqual "pattern refutability" (expectedRefutability patternValue) (patternRefutability facts)
      case patternValue of
        PVariable _ name -> assertEqual "pattern variable has a resolved type" True (Map.member name (patternBindingTypes facts))
        PConstructor _ name _ -> assertEqual "pattern constructor identity" (PatternConstructor name) (patternConstructorFact facts)
        _ -> pure ()
  case patternValue of
    PConstructor _ _ patterns -> mapM_ assertPatternFacts patterns
    PList _ patterns -> mapM_ assertPatternFacts patterns
    PConsList _ headPattern tailPattern -> mapM_ assertPatternFacts [headPattern, tailPattern]
    PTuple _ patterns -> mapM_ assertPatternFacts patterns
    PAs _ name nested -> do
      case patternNode patternValue of
        CoreNode _ _ facts -> assertEqual "as-pattern binder has a resolved type" True (Map.member name (patternBindingTypes facts))
      assertPatternFacts nested
    POr _ alternatives -> mapM_ assertPatternFacts alternatives
    _ -> pure ()

expectedRefutability :: Pattern phase -> PatternRefutability
expectedRefutability patternValue =
  case patternValue of
    PWildcard {} -> IrrefutablePattern
    PVariable {} -> IrrefutablePattern
    PAs _ _ nested -> expectedRefutability nested
    PTuple _ patterns
      | all ((== IrrefutablePattern) . expectedRefutability) patterns -> IrrefutablePattern
    _ -> RefutablePattern

assertStatementFacts :: Statement 'Analyzed -> IO ()
assertStatementFacts statement = do
  case statementCoreNode statement of
    CoreNode _ _ facts ->
      case statement of
        SLet _ name _ -> assertBindingStatement (ValueDeclaration name) facts
        SSignature _ name _ -> assertBindingStatement (SignatureDeclaration name) facts
        SData _ name _ constructors -> do
          assertEqual "data declaration fact" (DataDeclaration name [constructorName | DataConstructor _ constructorName _ <- constructors]) (statementDeclarationFact facts)
          mapM_ assertConstructorFacts constructors
        SClass _ name parameters methods -> do
          assertEqual "capability declaration fact" (CapabilityDeclaration name parameters) (statementDeclarationFact facts)
          mapM_ assertClassMethodFacts methods
        SImpl _ name _ methods -> do
          case statementDeclarationFact facts of
            ImplementationDeclaration factName [_] -> assertEqual "implementation declaration identity" name factName
            other -> fail ("missing analyzed implementation target: " <> show other)
          mapM_ assertImplMethodFacts methods
        SModule _ path -> assertEqual "module declaration fact" (ModuleDeclaration path) (statementDeclarationFact facts)
        SImport _ path _ _ -> assertEqual "import declaration fact" (ImportDeclaration path) (statementDeclarationFact facts)
        SExpr _ value -> assertEqual "expression declaration fact" ExpressionDeclaration (statementDeclarationFact facts) >> assertExprFacts value
  case statement of
    SLet _ _ value -> assertExprFacts value
    _ -> pure ()
  where
    assertBindingStatement expected facts = do
      assertEqual "statement declaration identity" expected (statementDeclarationFact facts)
      assertEqual "statement owns one binder" 1 (length (statementBinderIds facts))
      assertEqual "statement binder owns a generalized scheme" (Set.fromList (statementBinderIds facts)) (Map.keysSet (statementGeneralizedSchemes facts))
    assertConstructorFacts (DataConstructor (CoreNode _ _ facts) name _) = assertBindingStatement (ValueDeclaration name) facts
    assertClassMethodFacts (ClassMethodSignature (CoreNode _ _ facts) name _) =
      case statementDeclarationFact facts of
        MethodDeclaration factName _ -> assertEqual "class method declaration identity" name factName
        other -> fail ("missing analyzed method signature: " <> show other)
    assertImplMethodFacts (ImplMethod (CoreNode _ _ facts) name body) = do
      assertEqual "impl method declaration fact" (ValueDeclaration name) (statementDeclarationFact facts)
      assertExprFacts body

exprNode :: Expr phase -> CoreNode phase 'ExpressionSort
exprNode expression =
  case expression of
    ELit node _ -> node
    EVar node _ -> node
    ELambda node _ _ -> node
    EOperatorValue node _ -> node
    EList node _ -> node
    ETuple node _ -> node
    EApply node _ _ -> node
    ETypeApplication node _ _ _ -> node
    EIf node _ _ _ -> node
    EPatternCase node _ _ -> node
    EBinary node _ _ _ -> node
    ESectionLeft node _ _ -> node
    ESectionRight node _ _ -> node
    EBlock node _ -> node

patternNode :: Pattern phase -> CoreNode phase 'PatternSort
patternNode patternValue =
  case patternValue of
    PWildcard node -> node
    PVariable node _ -> node
    PLiteral node _ -> node
    PConstructor node _ _ -> node
    PList node _ -> node
    PConsList node _ _ -> node
    PTuple node _ -> node
    PAs node _ _ -> node
    POr node _ -> node

statementCoreNode :: Statement phase -> CoreNode phase 'StatementSort
statementCoreNode statement =
  case statement of
    SLet node _ _ -> node
    SSignature node _ _ -> node
    SData node _ _ _ -> node
    SClass node _ _ _ -> node
    SImpl node _ _ _ -> node
    SModule node _ -> node
    SImport node _ _ _ -> node
    SExpr node _ -> node

factCompletenessSources :: Map.Map FilePath Text
factCompletenessSources =
  Map.fromList
    [ ( "src/App/Main.jz",
        """
        module App::Main (result) {
        import Lib::Facts.
        result = identity @Int (case Box 1 { | Box item -> if Eq::equals item 1 then item else 0 }).
        result.
        }
        """
      ),
      ( "src/Lib/Facts.jz",
        """
        module Lib::Facts (identity, countdown, increment, type Box(Box), Eq) {
        identity :: a -> a.
        identity = \\(item) -> item.
        countdown :: Int -> Int.
        countdown = \\(number) -> if number == 0 then 0 else countdown (number - 1).
        increment = \\(number) -> number + 1.
        data Box a = Box a.
        class Eq(a) { equals :: a -> a -> Bool. }.
        impl Eq(Int) { equals = \\(left, right) -> left == right. }.
        }
        """
      )
    ]

testAnalyzedGenericConstructorFieldsRemainModuleStable :: IO ()
testAnalyzedGenericConstructorFieldsRemainModuleStable = do
  (_, analyzed) <- analyzeFixtureProgram sources
  case lookupCoreModule (nominalModulePath ("Lib" :| ["Box"])) analyzed of
    Nothing -> fail "missing analyzed Lib::Box module"
    Just boxModule ->
      case Map.lookup "Box" (interfaceDataTypes (analyzedInterface boxModule)) of
        Just
          ( DataTypeBinding
              [_]
              [[ConstructorArgumentType (SemanticList (SemanticVariable parameterName))]]
            ) ->
            assertEqual "stable constructor parameter name" "a" parameterName
        binding ->
          fail ("unexpected analyzed Box constructor metadata: " <> show binding)
  case evaluateAnalyzedProgram analyzed of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      assertEqual
        "cross-module structured constructor output"
        (Just "Box([1])")
        (renderRuntimeValue <$> runtimeProgramOutput runtime)
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Box. Box [1]. }"),
          ("src/Lib/Box.jz", "module Lib::Box { data Box a = Box [a]. }")
        ]

testRuntimeUsesAnalyzedDeclarations :: IO ()
testRuntimeUsesAnalyzedDeclarations = do
  (_, analyzed) <-
    analyzeFixtureProgram
      (Map.singleton "src/App/Main.jz" "module App::Main { data Box a = Box a. class Echo(a) { echo :: a -> a. }. impl Echo(Int) { echo = \\(item) -> item. }. result = Echo::echo 7. (Box result, result). }")
  let expression = coreModuleExpr (NonEmpty.head (coreProgramModules analyzed))
      rendered = fmap (fmap renderRuntimeValue) . evaluateRuntimeExpr
  assertEqual "analyzed declarations evaluate" (Right (Just "(Box(7), 7)")) (rendered expression)
  assertEqual "source type erasure does not change execution" (rendered expression) (rendered (erase expression))
  where
    erase (EBlock node statements) = EBlock node (map eraseStatement statements)
    erase expression = expression
    eraseStatement statement = case statement of
      SData node name parameters constructors ->
        SData node name parameters [DataConstructor child constructorName [] | DataConstructor child constructorName _ <- constructors]
      SClass node name parameters methods ->
        SClass node name parameters [ClassMethodSignature child methodName (SignatureType TypeBool) | ClassMethodSignature child methodName _ <- methods]
      SImpl node name _ methods -> SImpl node name [] methods
      _ -> statement

testLexicalBindersShadowImportedAndBuiltinNames :: IO ()
testLexicalBindersShadowImportedAndBuiltinNames = do
  (_, analyzed) <- analyzeFixtureProgram sources
  case evaluateAnalyzedProgram analyzed of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      assertEqual
        "lexical shadowing output"
        (Just "(1, 2, 3, 4)")
        (renderRuntimeValue <$> runtimeProgramOutput runtime)
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            module App::Main {
              import Lib::Value.
              ((\\(x) -> x) 1, case 2 { | x -> x }, { x = 3. x. }, (\\(map) -> map) 4).
            }
            """
          ),
          ( "src/Lib/Value.jz",
            """
            module Lib::Value {
              x = 99.
            }
            """
          )
        ]

testExplicitInstantiationBinderShadowing :: IO ()
testExplicitInstantiationBinderShadowing = do
  (_, analyzed) <- analyzeFixtureProgram sources
  coreModule <-
    maybe
      (fail "missing analyzed App::Main module")
      pure
      (lookupCoreModule (nominalModulePath ("App" :| ["Main"])) analyzed)
  let identityBinderIds = identityDefinitionBinderIds (coreModuleExpr coreModule)
      instantiations = expressionInstantiationInventory (coreModuleExpr coreModule)
      instantiatedBinderTypes =
        [ (binder, instantiatedType)
        | SemanticInstantiation (LexicalInstantiation binder) (instantiatedType :| []) <- instantiations
        ]
      runtimeInstantiations = runtimeInstantiationInventory (coreModuleExpr coreModule)
  assertEqual "two lexical identity definitions" 2 (length identityBinderIds)
  assertEqual
    "explicit applications reference their lexical definition-node binders"
    (Set.fromList [(identityBinderIds !! 0, SemanticInt), (identityBinderIds !! 1, SemanticBool)])
    (Set.fromList instantiatedBinderTypes)
  assertEqual
    "runtime plans retain both exact type instantiations"
    (Set.fromList [SemanticInt :| [], SemanticBool :| []])
    (Set.fromList runtimeInstantiations)
  where
    sources =
      Map.singleton
        "src/App/Main.jz"
        """
        module App::Main {
          identity = \\(item) -> item.
          outer = identity @Int 1.
          nested = {
            identity = \\(item) -> item.
            identity @Bool True.
          }.
          (outer, nested).
        }
        """

testExplicitOperatorInstantiationBinder :: IO ()
testExplicitOperatorInstantiationBinder = do
  (_, analyzed) <-
    analyzeFixtureProgram
      ( Map.singleton
          "src/App/Main.jz"
          """
          module App::Main {
            operator %% tier 2.
            (%%) :: a -> a -> a.
            (%%) = \\(left, right) -> left.
            result = (%%) @Int 1 2.
            result.
          }
          """
      )
  coreModule <-
    maybe
      (fail "missing analyzed App::Main module")
      pure
      (lookupCoreModule (nominalModulePath ("App" :| ["Main"])) analyzed)
  let operatorBinders =
        [ binder
        | SLet (CoreNode _ _ facts) name _ <- moduleStatements (coreModuleExpr coreModule),
          name == operatorBindingName "%%",
          binder <- statementBinderIds facts
        ]
      instantiatedBinders =
        [ binder
        | SemanticInstantiation (LexicalInstantiation binder) _ <- expressionInstantiationInventory (coreModuleExpr coreModule)
        ]
  assertEqual "one operator definition binder" 1 (length operatorBinders)
  assertEqual "explicit operator application references its definition binder" operatorBinders instantiatedBinders
  where
    moduleStatements expression =
      case expression of
        EBlock _ statements -> statements
        _ -> []

testStatementSchemesAreDefinitionSiteFacts :: IO ()
testStatementSchemesAreDefinitionSiteFacts = do
  (_, analyzed) <-
    analyzeFixtureProgram
      (Map.singleton "src/App/Main.jz" "module App::Main { x = 1. x = True. x. }")
  coreModule <-
    maybe
      (fail "missing analyzed App::Main module")
      pure
      (lookupCoreModule (nominalModulePath ("App" :| ["Main"])) analyzed)
  case namedLetSchemes "x" (coreModuleExpr coreModule) of
    [firstScheme, secondScheme] -> do
      assertEqual "first rebinding scheme is numeric" True (isNumericType (analyzedSchemeType firstScheme))
      assertEqual "second rebinding scheme is Bool" SemanticBool (analyzedSchemeType secondScheme)
    schemes -> fail ("expected two definition-site x schemes, got " <> show schemes)
  where
    isNumericType expressionType =
      case expressionType of
        SemanticInt -> True
        SemanticNumeric _ -> True
        _ -> False

testBuiltinAliasStatementScheme :: IO ()
testBuiltinAliasStatementScheme = do
  (_, analyzed) <-
    analyzeFixtureProgram
      (Map.singleton "src/App/Main.jz" "module App::Main { alias = __kernel_toInt8. alias. }")
  coreModule <-
    maybe
      (fail "missing analyzed App::Main module")
      pure
      (lookupCoreModule (nominalModulePath ("App" :| ["Main"])) analyzed)
  case namedLetSchemes "alias" (coreModuleExpr coreModule) of
    [scheme] -> do
      case (analyzedSchemeVariables scheme, analyzedSchemePrimitiveConstraints scheme, analyzedSchemeType scheme) of
        ( [variable],
          [AnalyzedNumericPrimitiveConstraint AnalyzedAnyNumericConstraint (SemanticVariable constrainedVariable)],
          SemanticFunction (SemanticVariable sourceVariable) (SemanticNumeric NumericInt8)
          ) ->
            assertEqual
              "builtin alias quantifies the exact numeric source variable"
              (variable, variable)
              (constrainedVariable, sourceVariable)
        facts -> fail ("builtin alias did not retain its full conversion scheme: " <> show facts)
      assertEqual "builtin alias has no class constraints" [] (analyzedSchemeConstraints scheme)
    schemes -> assertEqual "builtin alias owns exactly one meaningful scheme" 1 (length schemes)

testSignedBuiltinAliasStatementScheme :: IO ()
testSignedBuiltinAliasStatementScheme = do
  (_, analyzed) <-
    analyzeFixtureProgram
      ( Map.singleton
          "src/App/Main.jz"
          "module App::Main { prepend :: a -> [a] -> [a]. prepend = __kernel_listPrependRaw. prepend. }"
      )
  coreModule <-
    maybe
      (fail "missing analyzed App::Main module")
      pure
      (lookupCoreModule (nominalModulePath ("App" :| ["Main"])) analyzed)
  case namedLetSchemes "prepend" (coreModuleExpr coreModule) of
    [scheme] ->
      case (analyzedSchemeVariables scheme, analyzedSchemePrimitiveConstraints scheme, analyzedSchemeType scheme) of
        ( [variable],
          [],
          SemanticFunction
            (SemanticVariable argumentVariable)
            (SemanticFunction (SemanticList (SemanticVariable listVariable)) (SemanticList (SemanticVariable resultVariable)))
          ) ->
            assertEqual
              "signed builtin alias preserves one authored type variable"
              (variable, variable, variable)
              (argumentVariable, listVariable, resultVariable)
        facts -> fail ("signed builtin alias did not retain its authored scheme: " <> show facts)
    schemes -> assertEqual "signed builtin alias owns exactly one meaningful scheme" 1 (length schemes)

identityDefinitionBinderIds :: Expr 'Analyzed -> [CoreBinderId]
identityDefinitionBinderIds expression =
  case expression of
    ELambda _ _ body -> identityDefinitionBinderIds body
    EList _ values -> foldMap identityDefinitionBinderIds values
    ETuple _ values -> foldMap identityDefinitionBinderIds values
    EApply _ function argument -> identityDefinitionBinderIds function <> identityDefinitionBinderIds argument
    ETypeApplication _ function _ _ -> identityDefinitionBinderIds function
    EIf _ condition whenTrue whenFalse -> foldMap identityDefinitionBinderIds [condition, whenTrue, whenFalse]
    EPatternCase _ scrutinee arms -> identityDefinitionBinderIds scrutinee <> foldMap armIds arms
    EBinary _ _ left right -> identityDefinitionBinderIds left <> identityDefinitionBinderIds right
    ESectionLeft _ left _ -> identityDefinitionBinderIds left
    ESectionRight _ _ right -> identityDefinitionBinderIds right
    EBlock _ statements -> foldMap statementIds statements
    _ -> []
  where
    armIds (CaseArm _ _ guard body) = foldMap identityDefinitionBinderIds guard <> identityDefinitionBinderIds body
    statementIds statement =
      case statement of
        SLet (CoreNode _ _ facts) name value ->
          [binder | identifierText name == "identity", binder <- statementBinderIds facts]
            <> identityDefinitionBinderIds value
        SImpl _ _ _ methods -> foldMap (\(ImplMethod _ _ body) -> identityDefinitionBinderIds body) methods
        SExpr _ value -> identityDefinitionBinderIds value
        _ -> []

runtimeInstantiationInventory :: Expr 'Analyzed -> [NonEmpty AnalyzedType]
runtimeInstantiationInventory expression =
  nodeInstantiations expression
    <> case expression of
      ELambda _ _ body -> runtimeInstantiationInventory body
      EList _ values -> foldMap runtimeInstantiationInventory values
      ETuple _ values -> foldMap runtimeInstantiationInventory values
      EApply _ function argument -> runtimeInstantiationInventory function <> runtimeInstantiationInventory argument
      ETypeApplication _ function _ _ -> runtimeInstantiationInventory function
      EIf _ condition whenTrue whenFalse -> foldMap runtimeInstantiationInventory [condition, whenTrue, whenFalse]
      EPatternCase _ scrutinee arms -> runtimeInstantiationInventory scrutinee <> foldMap armInstantiations arms
      EBinary _ _ left right -> runtimeInstantiationInventory left <> runtimeInstantiationInventory right
      ESectionLeft _ left _ -> runtimeInstantiationInventory left
      ESectionRight _ _ right -> runtimeInstantiationInventory right
      EBlock _ statements -> foldMap statementInstantiations statements
      _ -> []
  where
    nodeInstantiations value =
      case exprNode value of
        CoreNode _ _ facts ->
          [types | InstantiateTypes types <- toList obligations]
          where
            RuntimePlan obligations = expressionRuntimePlan facts
    armInstantiations (CaseArm (CoreNode _ _ facts) _ guard body) =
      [types | InstantiateTypes types <- toList obligations]
        <> foldMap runtimeInstantiationInventory guard
        <> runtimeInstantiationInventory body
      where
        RuntimePlan obligations = expressionRuntimePlan facts
    statementInstantiations statement =
      case statement of
        SLet _ _ value -> runtimeInstantiationInventory value
        SImpl _ _ _ methods -> foldMap (\(ImplMethod _ _ body) -> runtimeInstantiationInventory body) methods
        SExpr _ value -> runtimeInstantiationInventory value
        _ -> []

namedLetSchemes :: Text -> Expr 'Analyzed -> [AnalyzedScheme]
namedLetSchemes expectedName expression =
  case expression of
    EBlock _ statements -> foldMap statementSchemes statements
    _ -> []
  where
    statementSchemes :: Statement 'Analyzed -> [AnalyzedScheme]
    statementSchemes statement =
      case statement of
        SLet (CoreNode _ _ facts) name _
          | identifierText name == expectedName -> Map.elems (statementGeneralizedSchemes facts)
        _ -> []

testRuntimeModulePublishesDeclaredExports :: IO ()
testRuntimeModulePublishesDeclaredExports = do
  (_, analyzed) <- analyzeFixtureProgram simpleSources
  case evaluateAnalyzedProgram analyzed of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Value"] runtime of
        Nothing -> fail "missing runtime Lib::Value module"
        Just runtimeModule ->
          assertEqual
            "export names"
            (Set.fromList [RuntimeBindingExport (ModuleExport ValueNamespace "answer")])
            (Map.keysSet (runtimeModuleExports runtimeModule))

testAnalyzedModuleKeepsPrivateInterfaceWithPublicInventory :: IO ()
testAnalyzedModuleKeepsPrivateInterfaceWithPublicInventory = do
  (_, analyzed) <- analyzeFixtureProgram explicitExportSources
  case lookupCoreModule (nominalModulePath ("Lib" :| ["Value"])) analyzed of
    Nothing -> fail "missing analyzed Lib::Value module"
    Just valueModule -> do
      assertEqual
        "full analyzed interface"
        (Set.fromList [ModuleExport ValueNamespace "answer", ModuleExport ValueNamespace "helper"])
        (Map.keysSet (interfaceValueBindings (analyzedInterface valueModule)))
      assertEqual
        "public analyzed inventory"
        (Set.singleton (ModuleExport ValueNamespace "answer"))
        ( exportInventoryEntries
            (analyzedExportInventory valueModule)
        )

testRuntimeModulePublishesExplicitExportsOnly :: IO ()
testRuntimeModulePublishesExplicitExportsOnly = do
  (_, analyzed) <- analyzeFixtureProgram explicitExportSources
  case evaluateAnalyzedProgram analyzed of
    Left diagnostic -> fail (Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Value"] runtime of
        Nothing -> fail "missing runtime Lib::Value module"
        Just runtimeModule ->
          assertEqual
            "public runtime exports"
            (Set.singleton (RuntimeBindingExport (ModuleExport ValueNamespace "answer")))
            (Map.keysSet (runtimeModuleExports runtimeModule))

testRuntimeModulePublishesPublicClassMethodsOnly :: IO ()
testRuntimeModulePublishesPublicClassMethodsOnly = do
  (_, analyzed) <- analyzeFixtureProgram explicitCapabilitySources
  case evaluateAnalyzedProgram analyzed of
    Left diagnostic -> fail (Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Facts"] runtime of
        Nothing -> fail "missing runtime Lib::Facts module"
        Just runtimeModule ->
          assertEqual
            "public class method runtime exports"
            (Set.singleton (RuntimeCapabilityMethodExport "Eq" "equals"))
            (Map.keysSet (runtimeModuleExports runtimeModule))

explicitExportSources :: Map.Map FilePath Text
explicitExportSources =
  Map.fromList
    [ ( "src/App/Main.jz",
        """
        module App::Main {
        import Lib::Value (answer).
        answer 41.
        }
        """
      ),
      ( "src/Lib/Value.jz",
        """
        module Lib::Value (answer) {
        helper = \\(x) -> x + 1.
        answer = \\(x) -> helper x.
        }
        """
      )
    ]

explicitCapabilitySources :: Map.Map FilePath Text
explicitCapabilitySources =
  Map.fromList
    [ ( "src/App/Main.jz",
        """
        module App::Main {
        import Lib::Facts (Eq).
        Eq::equals 1 1.
        }
        """
      ),
      ( "src/Lib/Facts.jz",
        """
        module Lib::Facts (Eq) {
        class Eq(a) {
        equals :: a -> a -> Bool.
        }.
        class Hidden(a) {
        secret :: a -> Bool.
        }.
        impl Eq(Int) {
        equals = \\(left, right) -> True.
        }.
        impl Hidden(Int) {
        secret = \\(item) -> False.
        }.
        }
        """
      )
    ]

testModuleExportIdentityPreservesNamespaces :: IO ()
testModuleExportIdentityPreservesNamespaces = do
  (_, analyzed) <- analyzeFixtureProgram shadowingSources
  case lookupCoreModule (nominalModulePath ("Lib" :| ["Maybe"])) analyzed of
    Nothing -> fail "missing analyzed Lib::Maybe module"
    Just maybeModule -> do
      let bindings = Map.filterWithKey (\moduleExport _ -> moduleExportName moduleExport == "Just") (interfaceValueBindings (analyzedInterface maybeModule))
          binder namespace = interfaceBindingId <$> Map.lookup (ModuleExport namespace "Just") bindings
      assertEqual "analyzed shadowed export identities" expectedExports (Map.keysSet bindings)
      case coreModuleStatements maybeModule of
        [SData _ _ _ [DataConstructor constructorNode _ _], SLet valueNode _ _] -> do
          assertEqual
            "constructor interface retains its declaration ID"
            (resolvedNodeBinder (statementResolution (coreNodeFacts constructorNode)))
            (binder ConstructorNamespace)
          assertEqual
            "value interface retains its distinct declaration ID"
            (resolvedNodeBinder (statementResolution (coreNodeFacts valueNode)))
            (binder ValueNamespace)
        _ -> fail "unexpected constructor/value declaration fixture"
      case lookupCoreModule (nominalModulePath ("App" :| ["Main"])) analyzed of
        Just entry
          | [SExpr _ (EVar node _)] <- coreModuleStatements entry ->
              assertEqual
                "imported use retains the interface declaration ID"
                (LexicalReference <$> binder ValueNamespace)
                (resolvedNodeReference (expressionResolution (coreNodeFacts node)))
        _ -> fail "unexpected imported value fixture"
  case evaluateAnalyzedProgram analyzed of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Maybe"] runtime of
        Nothing -> fail "missing runtime Lib::Maybe module"
        Just runtimeModule ->
          assertEqual
            "runtime shadowed export identities"
            expectedRuntimeExports
            ( Map.keysSet
                ( Map.filterWithKey
                    ( \runtimeExport _ ->
                        case runtimeExport of
                          RuntimeBindingExport moduleExport -> moduleExportName moduleExport == "Just"
                          RuntimeCapabilityMethodExport {} -> False
                    )
                    (runtimeModuleExports runtimeModule)
                )
            )
  where
    expectedExports =
      Set.fromList
        [ ModuleExport ValueNamespace "Just",
          ModuleExport ConstructorNamespace "Just"
        ]
    expectedRuntimeExports = Set.map RuntimeBindingExport expectedExports
    shadowingSources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Maybe (Just). Just. }"),
          ("src/Lib/Maybe.jz", "module Lib::Maybe { data Maybe a = Just a. Just = 1. }")
        ]

testNamespaceAwareRuntimeExportPublishesValueOnly :: IO ()
testNamespaceAwareRuntimeExportPublishesValueOnly = do
  (_, analyzed) <- analyzeFixtureProgram sources
  case evaluateAnalyzedProgram analyzed of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Maybe"] runtime of
        Nothing -> fail "missing runtime Lib::Maybe module"
        Just runtimeModule ->
          assertEqual
            "namespace-selected runtime exports"
            (Set.singleton (RuntimeBindingExport (ModuleExport ValueNamespace "Just")))
            (Map.keysSet (runtimeModuleExports runtimeModule))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Maybe (Just). Just. }"),
          ("src/Lib/Maybe.jz", "module Lib::Maybe (value Just) { data Maybe a = Just a. Just = 1. }")
        ]

testNamespaceAwareRuntimeExportPublishesConstructorOnly :: IO ()
testNamespaceAwareRuntimeExportPublishesConstructorOnly = do
  (_, analyzed) <- analyzeFixtureProgram sources
  case evaluateAnalyzedProgram analyzed of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Maybe"] runtime of
        Nothing -> fail "missing runtime Lib::Maybe module"
        Just runtimeModule ->
          assertEqual
            "namespace-selected constructor runtime export"
            (Set.singleton (RuntimeBindingExport (ModuleExport ConstructorNamespace "Just")))
            (Map.keysSet (runtimeModuleExports runtimeModule))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Maybe (Just). Just. }"),
          ("src/Lib/Maybe.jz", "module Lib::Maybe (constructor Just) { data Maybe a = Just a. Just = 1. }")
        ]

testGroupedExportsPublishSelectedConstructor :: IO ()
testGroupedExportsPublishSelectedConstructor = do
  (_, analyzed) <- analyzeFixtureProgram sources
  case lookupCoreModule (nominalModulePath ("Lib" :| ["Choice"])) analyzed of
    Nothing -> fail "missing analyzed Lib::Choice module"
    Just choiceModule ->
      do
        assertEqual
          "full grouped analyzed interface retains private constructors"
          ( Set.fromList
              [ ModuleExport ConstructorNamespace "First",
                ModuleExport ConstructorNamespace "Second"
              ]
          )
          (Map.keysSet (interfaceValueBindings (analyzedInterface choiceModule)))
        assertEqual
          "grouped public inventory"
          ( Set.fromList
              [ ModuleExport ConstructorNamespace "First",
                ModuleExport TypeNamespace "Choice"
              ]
          )
          ( exportInventoryEntries
              (analyzedExportInventory choiceModule)
          )
  case evaluateAnalyzedProgram analyzed of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Choice"] runtime of
        Nothing -> fail "missing runtime Lib::Choice module"
        Just runtimeModule ->
          assertEqual
            "grouped runtime export inventory"
            (Set.singleton (RuntimeBindingExport (ModuleExport ConstructorNamespace "First")))
            (Map.keysSet (runtimeModuleExports runtimeModule))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Choice. First 1. }"),
          ("src/Lib/Choice.jz", "module Lib::Choice (type Choice(First)) { data Choice a = First a | Second a. }")
        ]

testAnalyzedDependencyTerminalExpressionIsSkipped :: IO ()
testAnalyzedDependencyTerminalExpressionIsSkipped = do
  (_, analyzed) <- analyzeFixtureProgram dependencyExpressionSources
  case evaluateAnalyzedProgram analyzed of
    Left diagnostic -> fail ("runtime program failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      assertEqual
        "entry output"
        (Just "1")
        (renderRuntimeValue <$> runtimeProgramOutput runtime)

testModuleRuntimePathParity :: IO ()
testModuleRuntimePathParity = do
  hostFreeProgram@(_, hostFreeAnalyzed) <- analyzeFixtureProgram hostFreeParitySources
  hostCapableProgram@(_, hostCapableAnalyzed) <- analyzeFixtureProgram hostCapableParitySources
  assertAbsentPrelude "host-free program" hostFreeProgram
  assertAbsentPrelude "host-capable program" hostCapableProgram
  assertEqual
    "host-free module requirements select the pure path"
    [False, False]
    (map (runtimeExprRequiresHost . coreModuleExpr) (NonEmpty.toList (coreProgramModules hostFreeAnalyzed)))
  assertEqual
    "unselected host call selects the host-capable path"
    [False, True]
    (map (runtimeExprRequiresHost . coreModuleExpr) (NonEmpty.toList (coreProgramModules hostCapableAnalyzed)))
  case (evaluateFixtureProgram hostFreeProgram, evaluateFixtureProgram hostCapableProgram) of
    (Right hostFreeRuntime, Right hostCapableRuntime) -> do
      let hostFreeProjection = observableRuntimeProgram hostFreeRuntime
          hostCapableProjection = observableRuntimeProgram hostCapableRuntime
          expectedProjection =
            ( ["Lib::Value", "App::Main"],
              [ ( "Lib::Value",
                  [ (RuntimeBindingExport (ModuleExport ConstructorNamespace "Other"), "Other"),
                    (RuntimeBindingExport (ModuleExport ConstructorNamespace "Shared"), "Shared")
                  ]
                ),
                ("App::Main", [])
              ],
              Just "(Shared, Other)"
            )
      assertEqual "host-free observable module result" expectedProjection hostFreeProjection
      assertEqual "host-capable observable module result" expectedProjection hostCapableProjection
      assertEqual "pure and host-capable observable module parity" hostFreeProjection hostCapableProjection
    (Left diagnostic, _) -> fail ("host-free runtime failed: " <> Text.unpack (renderDiagnostic diagnostic))
    (_, Left diagnostic) -> fail ("host-capable runtime failed: " <> Text.unpack (renderDiagnostic diagnostic))

testRunResultProjectionInvariants :: IO ()
testRunResultProjectionInvariants =
  mapM_ assertProjection cases
  where
    assertProjection (label, action, expected) = do
      result <- action
      assertEqual label expected (runResultProjection result)
    cases =
      [ ( "not executed",
          runProjectionFixture disabledRuntimeHost "module App::Main { missing. }",
          ("not-executed", Nothing, Nothing, Nothing)
        ),
        ( "runtime failed",
          runProjectionFixture disabledRuntimeHost "module App::Main { 1 / 0. }",
          ("runtime-failed", Nothing, Nothing, Nothing)
        ),
        ( "explicit exit",
          runProjectionFixture productionRuntimeHost "module App::Main { __kernel_exit! 7. }",
          ("exited", Nothing, Just 7, Nothing)
        ),
        ( "completed with value",
          runProjectionFixture disabledRuntimeHost "module App::Main { 42. }",
          ("completed", Just "42", Nothing, Just "42")
        ),
        ( "completed without terminal value",
          runProjectionFixture disabledRuntimeHost "module App::Main { answer = 42. }",
          ("completed", Nothing, Nothing, Nothing)
        )
      ]

runResultProjection :: RunResult -> (Text, Maybe Text, Maybe Integer, Maybe Text)
runResultProjection result =
  ( runExecutionTag (runExecution result),
    renderRuntimeValue <$> runRuntimeValue result,
    runExitStatus result,
    runOutput result
  )

runExecutionTag :: RunExecution -> Text
runExecutionTag execution =
  case execution of
    RunNotExecuted -> "not-executed"
    RunRuntimeFailed -> "runtime-failed"
    RunExited _ -> "exited"
    RunCompleted _ -> "completed"

runProjectionFixture :: RuntimeHost IO -> Text -> IO RunResult
runProjectionFixture host source =
  runModuleGraphWithPreludeAndHost
    host
    defaultWarningSettings
    Nothing
    resolverConfig
    ["App", "Main"]
    (\path -> pure (Map.lookup path sources))
  where
    sources = Map.singleton "src/App/Main.jz" source

assertAbsentPrelude :: String -> (CoreProgram 'Resolved, CoreProgram 'Analyzed) -> IO ()
assertAbsentPrelude label (resolvedProgram, _) =
  case preludeModule (coreProgramPrelude resolvedProgram) of
    Nothing -> pure ()
    Just _ -> fail (label <> " unexpectedly contains a prelude")

observableRuntimeProgram :: RuntimeProgram -> ([Text], [(Text, [(RuntimeExport, Text)])], Maybe Text)
observableRuntimeProgram runtimeProgram =
  ( map (renderModulePath . runtimeModulePath) runtimeModules,
    map renderModuleExports runtimeModules,
    renderRuntimeValue <$> runtimeProgramOutput runtimeProgram
  )
  where
    runtimeModules = runtimeProgramModules runtimeProgram
    renderModuleExports runtimeModule =
      ( renderModulePath (runtimeModulePath runtimeModule),
        [(runtimeExport, renderRuntimeCell cell) | (runtimeExport, cell) <- Map.toAscList (runtimeModuleExports runtimeModule)]
      )

renderModulePath :: [Text] -> Text
renderModulePath = Text.intercalate "::"

renderRuntimeCell :: RuntimeCell -> Text
renderRuntimeCell cell =
  case cell of
    Left diagnostic -> renderDiagnostic diagnostic
    Right value -> renderRuntimeValue value

hostFreeParitySources :: Map.Map FilePath Text
hostFreeParitySources = moduleRuntimeParitySources "(Shared, Other)"

hostCapableParitySources :: Map.Map FilePath Text
hostCapableParitySources =
  moduleRuntimeParitySources
    "if True then (Shared, Other) else (\\(ignored) -> (Shared, Other)) (__kernel_arguments! ())"

moduleRuntimeParitySources :: Text -> Map.Map FilePath Text
moduleRuntimeParitySources entryExpression =
  Map.fromList
    [ ("src/App/Main.jz", "module App::Main { import Lib::Value. " <> entryExpression <> ". }"),
      ("src/Lib/Value.jz", "module Lib::Value { data Marker = Shared | Other. }")
    ]

testModuleGraphInjectsRuntimeHost :: IO ()
testModuleGraphInjectsRuntimeHost = do
  callsRef <- newIORef []
  result <-
    runModuleGraphWithPreludeAndHost
      (recordingHost callsRef)
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      (\path -> pure (Map.lookup path sources))
  calls <- readIORef callsRef
  assertEqual "host module compile errors" [] (runCompileErrors result)
  assertEqual "host module runtime errors" [] (runRuntimeErrors result)
  assertEqual "host module output" (Just "(True, \"\", \"\", \"\")") (runOutput result)
  assertEqual "host module call order" ["entry"] calls
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            "module App::Main { import Lib::Emit (emit!). emit! \"entry\". }"
          ),
          ( "src/Lib/Emit.jz",
            "module Lib::Emit (emit!) { emit! = \\(contents) -> __kernel_writeStdoutRaw! contents. }"
          )
        ]

recordingHost :: IORef [Text] -> RuntimeHost IO
recordingHost callsRef =
  RuntimeHost
    { runtimeHostReadText = \_ -> pure (error "unexpected readText host call"),
      runtimeHostWriteText = \_ _ -> pure (error "unexpected writeText host call"),
      runtimeHostReadStdin = pure (error "unexpected readStdin host call"),
      runtimeHostWriteStdout = \contents -> do
        modifyIORef' callsRef (<> [contents])
        pure (Right ()),
      runtimeHostWriteStderr = \_ -> pure (error "unexpected writeStderr host call"),
      runtimeHostArguments = pure [],
      runtimeHostExit = \_ -> pure (Right RuntimeHostExitReturned)
    }

analyzeFixtureProgram :: Map.Map FilePath Text -> IO (CoreProgram 'Resolved, CoreProgram 'Analyzed)
analyzeFixtureProgram sources = do
  resolved <- resolveFixtureProgram sources
  (diagnostics, maybeAnalyzed) <- analyzeProgram (emptyCompileInputs defaultWarningSettings) resolved
  case maybeAnalyzed of
    Nothing -> fail ("analysis failed: " <> show (map renderDiagnostic diagnostics))
    Just analyzed -> pure (resolved, analyzed)

evaluateFixtureProgram :: (CoreProgram 'Resolved, CoreProgram 'Analyzed) -> Either Diagnostic RuntimeProgram
evaluateFixtureProgram (_, analyzed) =
  runtimeOutcomeAsDiagnosticResult
    ( runtimeObservationOutcome
        (runIdentity (interpretAnalyzedProgram RuntimeObservationDisabled disabledRuntimeHost analyzed))
    )

analyzedInterface :: CoreModule 'Analyzed -> ModuleInterface
analyzedInterface = analyzedModuleInterface . coreModuleFacts

analyzedExportInventory :: CoreModule 'Analyzed -> ModuleExportInventory
analyzedExportInventory = analyzedModuleExports . coreModuleFacts

resolveFixtureProgram :: Map.Map FilePath Text -> IO (CoreProgram 'Resolved)
resolveFixtureProgram sources = do
  resolvedResult <-
    resolveProgramWithAmbientExports
      resolverConfig
      testPrelude
      (exportInventory [])
      (\path -> pure (Map.lookup path sources))
      ["App", "Main"]
  case resolvedResult of
    Left diagnostic -> fail ("resolution failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right resolved -> pure resolved

simpleSources :: Map.Map FilePath Text
simpleSources =
  Map.fromList
    [ ("src/App/Main.jz", "module App::Main { import Lib::Value. answer. }"),
      ("src/Lib/Value.jz", "module Lib::Value { answer = 1. }")
    ]

dependencyExpressionSources :: Map.Map FilePath Text
dependencyExpressionSources =
  Map.fromList
    [ ("src/App/Main.jz", "module App::Main { import Lib::Value. result. }"),
      ("src/Lib/Value.jz", "module Lib::Value { result = 1. 1 / 0. }")
    ]

testAnalyzedInterfacesExposeOnlyDeclaredExports :: IO ()
testAnalyzedInterfacesExposeOnlyDeclaredExports = do
  resolvedResult <-
    resolveProgramWithAmbientExports
      resolverConfig
      testPrelude
      (exportInventory [])
      lookupSource
      ["App", "Main"]
  case resolvedResult of
    Left diagnostic -> fail ("resolution failed: " <> Text.unpack (renderDiagnostic diagnostic))
    Right resolved -> do
      (diagnostics, maybeAnalyzed) <- analyzeProgram (emptyCompileInputs defaultWarningSettings) resolved
      analyzed <- maybe (fail "successful program did not produce analyzed core") pure maybeAnalyzed
      case lookupCoreModule (nominalModulePath ("Lib" :| ["Value"])) analyzed of
        Nothing -> fail "missing analyzed Lib::Value module"
        Just valueModule ->
          assertEqual
            "exported values"
            (Set.fromList [ModuleExport ValueNamespace "answer"])
            (Map.keysSet (interfaceValueBindings (analyzedInterface valueModule)))
      assertEqual "no compile errors" [] (analyzedProgramErrors analyzed)
      assertEqual "no diagnostics" [] diagnostics
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value. answer. }"),
          ("src/Lib/Value.jz", "module Lib::Value { answer = 1. }")
        ]
    lookupSource path = pure (Map.lookup path sources)

testDependencyExpressionContract :: IO ()
testDependencyExpressionContract = do
  result <- runGraph localDependencyExpressionSources
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "entry output" (Just "1") (runOutput result)
  where
    localDependencyExpressionSources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value. result. }"),
          ("src/Lib/Value.jz", "module Lib::Value { result = 1. 1 / 0. }")
        ]

testAliasIsolationContract :: IO ()
testAliasIsolationContract = do
  result <- runGraph sources
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime output" (Just "1") (runOutput result)
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main { import Lib::Value as Value. Value::answer. }"),
          ("src/Lib/Value.jz", "module Lib::Value { answer = 1. }")
        ]

testTransitiveVisibilityContract :: IO ()
testTransitiveVisibilityContract = do
  result <- compileGraph sources
  assertEqual "warning count" 0 (length (compileWarnings result))
  assertEqual "error count" 1 (length (compileErrors result))
  assertContains
    "unbound code"
    "E1001"
    (renderFirstCompileError result)
  assertContains
    "alias-hidden transitive export"
    "unbound variable 'subtract'"
    (renderFirstCompileError result)
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import App::UsesMath.
            subtract.
            """
          ),
          ( "src/App/UsesMath.jz",
            """
            import Lib::Math as Math.
            use = 0.
            """
          ),
          ("src/Lib/Math.jz", "subtract = 2.")
        ]

testSourcePathContract :: IO ()
testSourcePathContract = do
  result <- compileGraph sources
  assertEqual "error count" 1 (length (compileErrors result))
  assertContains
    "dependency primary source path"
    "src/Lib/Bad.jz:1:1"
    (renderFirstCompileError result)
  assertContains
    "dependency related source path"
    "related src/Lib/Bad.jz:2:1"
    (renderFirstCompileError result)
  where
    sources =
      Map.fromList
        [ ( "src/App/Main.jz",
            """
            import Lib::Bad (x).
            x.
            """
          ),
          ( "src/Lib/Bad.jz",
            """
            x :: Int.
            x = True.
            """
          )
        ]

runGraph :: Map.Map FilePath Text -> IO RunResult
runGraph sources =
  runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  where
    lookupSource path = pure (Map.lookup path sources)

compileGraph :: Map.Map FilePath Text -> IO CompileResult
compileGraph sources =
  compileModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  where
    lookupSource path = pure (Map.lookup path sources)

renderFirstCompileError :: CompileResult -> Text
renderFirstCompileError result =
  case compileErrors result of
    [] -> "<no compile error>"
    firstError : _ -> renderDiagnostic firstError

resolverConfig :: ModuleResolutionConfig
resolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}

testPrelude :: PreludeArtifact 'Resolved
testPrelude =
  PreludeArtifact
    { preludeIdentity =
        moduleIdentity
          (nominalModulePath ("Prelude" :| []))
          (mkSourceFile "<module-pipeline-test-prelude>"),
      preludeModule = Nothing
    }

nominalModulePath :: NonEmpty Text -> ModulePath
nominalModulePath = mkModulePath . fmap mkIdentifier
