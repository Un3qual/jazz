{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
import Jazz.Compiler.CoreIdentity (CapabilityId (..), CoreBinderId (..), ImplId (..), MethodId (..), ResolvedNodeFacts (..), ResolvedReference (..), ResolvedScopeFacts (..), emptyResolvedNodeFacts, resolvedNodeImportTarget)
import Jazz.Compiler.Diagnostics (Diagnostic, SourceSpan (..), isErrorDiagnostic)
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
  )
import Jazz.Compiler.ModuleCompiler
  ( analyzeProgram,
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
    analyzedProgramErrors,
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
    modulePathTextSegments,
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
    resolvedImportedName,
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
  ( AnalyzedNumericConstraint (..),
    AnalyzedPrimitiveConstraint (..),
    AnalyzedScheme (..),
    EvidenceReference (..),
    ExpressionFacts (..),
    InstantiationTarget (..),
    PatternConstructorFact (..),
    PatternFacts (..),
    PatternRefutability (..),
    SemanticFactInvariantFailure (..),
    SemanticInstantiation (..),
    StatementDeclarationFact (..),
    StatementFacts (..),
  )
import Jazz.Compiler.SourceProgram (parseAndLowerStandaloneSource)
import Jazz.Compiler.TypeInference (CheckedExpr (..), InferenceInputs (..), inferExpressionWork)
import Jazz.Compiler.TypeInference.Analyzed (draftExpressionNode, draftStatementNode, finalizeCheckedExpression)
import Jazz.Compiler.TypeInference.Result (inferredDiagnostics)
import Jazz.Compiler.TypeInference.Solver (freshIntegerLiteralType)
import Jazz.Compiler.TypeInference.State
  ( InferState (..),
    initialInferState,
  )
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    DataTypeBinding (..),
    InferenceVariable (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    SchemePrimitiveConstraint (..),
    ScopeCapabilityFacts (scopeClassMethodSignatures),
    SemanticBinding (..),
    SemanticScheme (..),
    SemanticType (..),
    emptyScopeCapabilityFacts,
    quantifiedVariablesFromPreferred,
  )
import Jazz.Compiler.TypeRepresentation
  ( NumericType (NumericInt8),
    SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertSingleDiagnosticCode,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "ModulePipelineContract" tests

tests :: [NamedTest]
tests =
  [ ("standalone source and prelude keep separate graph identities", testStandaloneProgramOwnership),
    ("standalone prelude expressions retain effects and terminal values", testStandalonePreludeExecution),
    ("single-module analysis consumes complete imported interfaces", testSingleModuleAnalysis),
    ("exported scheme parameters are independent of private solver allocation", testExportedSchemeParameterIdentity),
    ("imported monomorphic aliases retain declaration sharing", testImportedMonomorphicAliasSharing),
    ("nominal types retain identity across their module boundary", testNominalTypeIdentity),
    ("runtime consumes analyzed declarations after source types are erased", testRuntimeUsesAnalyzedDeclarations),
    ("analyzed operators use ordinary calls and checked types", testAnalyzedOperatorFunctions),
    ("analyzed expressions preserve literal-range constraints for numeric specialization", testAnalyzedLiteralRangeFacts),
    ("checked subtrees own their facts before finalization", testCheckedSubtreeOwnership),
    ("successful inference attaches complete analyzed facts", testAnalyzedProgramFactsAreComplete),
    ("checked method schemes identify used and unused class parameters", testCheckedMethodParameterIdentity),
    ("checked-tree finalization rejects incomplete semantic nodes", testAnalyzedFactInvariantFailures),
    ("dependency expressions are checked but not executed", testDependencyExpressionContract),
    ("analyzed interfaces expose only declared exports", testAnalyzedInterfacesExposeOnlyDeclaredExports),
    ("runtime modules publish only declared exports", testRuntimeModulePublishesDeclaredExports),
    ("analyzed modules retain private interfaces with public inventories", testAnalyzedModulePublishesOnlyPublicDeclarations),
    ("runtime modules publish explicit value exports only", testRuntimeModulePublishesExplicitExportsOnly),
    ("runtime modules publish methods only for public classes", testRuntimeModulePublishesPublicClassMethodsOnly),
    ("module export identities distinguish shadowed values and constructors", testModuleExportIdentityPreservesNamespaces),
    ("namespace-aware runtime exports publish selected value only", testNamespaceAwareRuntimeExportPublishesValueOnly),
    ("namespace-aware runtime exports publish selected constructor only", testNamespaceAwareRuntimeExportPublishesConstructorOnly),
    ("grouped exports publish selected constructors through interface and runtime inventories", testGroupedExportsPublishSelectedConstructor),
    ("analyzed generic constructor fields remain module-stable", testAnalyzedGenericConstructorFieldsRemainModuleStable),
    ("analyzed dependency terminal expressions are skipped", testAnalyzedDependencyTerminalExpressionIsSkipped),
    ("host-free and host-capable module paths preserve observable results", testModuleRuntimePathParity),
    ("scope storage preserves interleaved recursive definition sites", testScopeStorageDefinitionSites),
    ("deferred cells memoize within each closure invocation", testScopeStorageInvocationIdentity),
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
  (resolved, analyzed) <- analyzeFixtureProgram sources
  let inputs = emptyCompileInputs defaultWarningSettings
      entryPath = nominalModulePath ("App" :| ["Main"])
      interfaces = Map.fromList [(coreModulePath checked, analyzedModuleInterface facts) | checked <- NonEmpty.toList (coreProgramModules analyzed), let facts = coreModuleFacts checked]
  entry <- maybe (fail "missing resolved entry") pure (lookupCoreModule entryPath resolved)
  expected <- maybe (fail "missing analyzed entry") pure (lookupCoreModule entryPath analyzed)
  let factsInterface = interfaces Map.! nominalModulePath ("Lib" :| ["Facts"])
  assertEqual
    "dependency interface excludes private and transitive values"
    (Set.fromList [ModuleExport ValueNamespace "equals", ModuleExport ValueNamespace "identity", ModuleExport ConstructorNamespace "Box"])
    (Map.keysSet (interfaceValueBindings factsInterface))
  assertEqual
    "unreachable private type metadata remains module-owned"
    (Set.singleton (resolvedImportedName (nominalModulePath ("Lib" :| ["Facts"])) TypeNamespace (mkIdentifier "Box")))
    (Map.keysSet (interfaceDataTypes factsInterface))
  imports <- traverse (dependencyInterface (resolvedModuleImportScope (coreModuleFacts entry)) interfaces) (coreModuleImports entry)
  (inference, actual) <- analyzeModule inputs False (mconcat imports) entry
  assertEqual "single-module diagnostics" [] (inferredDiagnostics inference)
  assertEqual "single-module facts, binders and evidence match program analysis" (Just expected) actual
  runtime <- either (fail . show) pure (evaluateAnalyzedProgram analyzed)
  assertEqual "aliased and selective imports retain every view" (Just "1") (renderRuntimeValue <$> runtimeProgramOutput runtime)
  failing <- resolveFixtureProgram (Map.singleton "src/App/Main.jz" "module App::Main { 1 True. }")
  let failingEntry = NonEmpty.head (coreProgramModules failing)
  (programDiagnostics, _) <- analyzeProgram inputs failing
  (failedInference, failedModule) <- analyzeModule inputs False mempty failingEntry
  assertEqual "failed module has no analyzed artifact" Nothing failedModule
  assertEqual "single-module diagnostic order matches program analysis" programDiagnostics (inferredDiagnostics failedInference)
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main (result) { import Lib::Facts as Facts. import Lib::Facts (Box). result = Facts::identity @Int (case Facts::Box 1 { | Box item -> if Facts::Equatable::equals item 1 then item else 0 }). result. }"),
          ("src/Lib/Facts.jz", "module Lib::Facts (identity, type Box(Box), Equatable) { import Lib::Hidden. privateHelper = \\(item) -> item. identity :: a -> a. identity = \\(item) -> privateHelper item. data Box a = Box a. data Unused = Unused. class Equatable(a) { equals :: a -> a -> Bool. }. impl Equatable(Int) { equals = \\(left, right) -> __kernel_equals left (__kernel_add right hiddenZero). }. }"),
          ("src/Lib/Hidden.jz", "module Lib::Hidden { hiddenZero = 0. }")
        ]
    dependencyInterface scope interfaces importDecl =
      case Map.lookup (importedModule importDecl) interfaces of
        Nothing -> fail "missing dependency interface"
        Just interface -> pure (dependencyImportInterface scope (importedModule importDecl) interface)

testExportedSchemeParameterIdentity :: IO ()
testExportedSchemeParameterIdentity = do
  direct <- exportedPick ""
  shifted <- exportedPick "private = \\(item) -> item. "
  assertEqual "closed exported scheme is stable across private allocations" direct shifted
  where
    exportedPick prefix = do
      (_, analyzed) <- analyzeFixtureProgram (Map.singleton "src/App/Main.jz" ("module App::Main { " <> prefix <> "pick :: a -> b -> a. pick = \\(left, right) -> left. }"))
      let interface = analyzedInterface (NonEmpty.last (coreProgramModules analyzed))
      case Map.lookup (ModuleExport ValueNamespace "pick") (interfaceValueBindings interface) of
        Just binding -> pure (interfaceBindingType binding)
        Nothing -> fail "missing exported pick scheme"

testImportedMonomorphicAliasSharing :: IO ()
testImportedMonomorphicAliasSharing = do
  resolved <- resolveFixtureProgram (sources "True")
  (diagnostics, analyzed) <- analyzeProgram (emptyCompileInputs defaultWarningSettings) resolved
  assertSingleDiagnosticCode "monomorphic aliases reject inconsistent uses" "E2006" (filter isErrorDiagnostic diagnostics)
  assertEqual "inconsistent aliases have no analyzed program" Nothing analyzed
  _ <- analyzeFixtureProgram (sources "2")
  pure ()
  where
    sources second =
      Map.fromList
        [ ("src/Lib/Box.jz", "module Lib::Box { data Box a = Box a. make = Box. other = make. }"),
          ("src/Lib/Alias.jz", "module Lib::Alias { import Lib::Box. another = other. }"),
          ("src/App/Main.jz", "module App::Main { import Lib::Box. import Lib::Alias. first = make 1. another " <> second <> ". }")
        ]

testNominalTypeIdentity :: IO ()
testNominalTypeIdentity = do
  (_, program) <-
    analyzeFixtureProgram
      ( Map.fromList
          [ ("src/Lib/Box.jz", "module Lib::Box { data Box = Box. boxed = Box. }"),
            ("src/App/Main.jz", "module App::Main { import Lib::Box. copy = boxed. }")
          ]
      )
  defining <- exportedType program (nominalModulePath ("Lib" :| ["Box"])) "boxed"
  imported <- exportedType program (nominalModulePath ("App" :| ["Main"])) "copy"
  assertEqual "the defining and importing views name the same type" defining imported
  where
    exportedType program path name = do
      checked <- maybe (fail "missing checked module") pure (lookupCoreModule path program)
      binding <- maybe (fail "missing exported value") pure (Map.lookup (ModuleExport ValueNamespace name) (interfaceValueBindings (analyzedInterface checked)))
      pure (interfaceBindingType binding)

testAnalyzedOperatorFunctions :: IO ()
testAnalyzedOperatorFunctions = do
  (_, analyzed) <-
    analyzeFixtureProgram
      (Map.singleton "src/App/Main.jz" "module App::Main { add = __kernel_add. equals = __kernel_equals. x :: Int8. x = 1. x == 2. x + 3. }")
  coreModule <-
    maybe
      (fail "missing analyzed module")
      pure
      (lookupCoreModule (nominalModulePath ("App" :| ["Main"])) analyzed)
  case coreModuleExpr coreModule of
    EBlock _ statements -> case [body | SExpr _ body <- statements] of
      [EApply comparison (EApply _ (EVar _ equalityName) _) _, EApply addition (EApply _ (EVar _ additionName) _) _] -> do
        assertEqual "comparison function" "equals" (identifierText equalityName)
        assertEqual "arithmetic function" "add" (identifierText additionName)
        assertEqual "comparison call result" SemanticBool (expressionSemanticType (coreNodeFacts comparison))
        assertEqual "arithmetic call preserves numeric width" (SemanticNumeric NumericInt8) (expressionSemanticType (coreNodeFacts addition))
      other -> fail ("expected ordinary calls: " <> show other)
    other -> fail ("expected checked block: " <> show other)

testAnalyzedLiteralRangeFacts :: IO ()
testAnalyzedLiteralRangeFacts = do
  let nodeId = CoreNodeId 17
      expression = ELit (CoreNode nodeId (SourceSpan 1 1) (emptyResolvedNodeFacts (NamedSourceUnit (nominalModulePath ("App" :| ["Main"]))))) (LInt 255)
      (literalType, literalState) = freshIntegerLiteralType (IntegerLiteralRange 0 255) initialInferState
      checked = CheckedExpr (Just literalType) (ELit <$> draftExpressionNode (Just literalType) expression <*> pure (LInt 255))
  case finalizeCheckedExpression literalState checked of
    Right (ELit (CoreNode _ _ facts) _) -> do
      assertEqual "uncommitted numeric representation" literalType (expressionSemanticType facts)
      assertEqual
        "analyzed facts retain the solver's complete literal range"
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
    [evidence@EvidenceReference {evidenceMethod = Just selectedMethod}] -> do
      assertEqual "capability evidence target" SemanticInt (evidenceType evidence)
      assertEqual
        "capability evidence preserves the selected canonical identities"
        (expectedEvidenceIdentities resolvedProgram)
        [(evidenceCapability evidence, evidenceImplementation evidence, selectedMethod)]
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
            ImportDeclaration target -> do
              assertEqual "import target identity" (importedModule importDecl) target
              assertEqual "import target survives checking" (Just target) (resolvedNodeImportTarget (statementResolution facts))
            declarationFact -> fail ("unexpected analyzed import declaration fact: " <> show declarationFact)

testCheckedMethodParameterIdentity :: IO ()
testCheckedMethodParameterIdentity = do
  (_, analyzed) <-
    analyzeFixtureProgram
      ( Map.singleton
          "src/App/Main.jz"
          "module App::Main { class Probe(a) { nested :: [a] -> [a]. constant :: Int -> Bool. }. 0. }"
      )
  let methods =
        Map.fromList
          [ (identifierText name, signature)
          | ((_, name), signature) <- Map.toList (scopeClassMethodSignatures (interfaceCapabilities (analyzedInterface (NonEmpty.head (coreProgramModules analyzed)))))
          ]
  case (Map.lookup "nested" methods, Map.lookup "constant" methods) of
    (Just nested, Just constant) -> do
      let parameter = SemanticVariable (classMethodParameter nested)
      assertEqual
        "nested occurrences refer to the explicit class parameter"
        (SemanticFunction (SemanticList parameter) (SemanticList parameter))
        (schemeResultType (classMethodScheme nested))
      assertEqual
        "a method can leave its class parameter unused"
        (SemanticFunction SemanticInt SemanticBool)
        (schemeResultType (classMethodScheme constant))
    _ -> fail "missing analyzed Probe methods"

-- Finalization may read the solver, but the checker must already own the tree
-- and its decisions. Erasing all output facts must leave that tree intact.
testCheckedSubtreeOwnership :: IO ()
testCheckedSubtreeOwnership = do
  let node number = CoreNode (CoreNodeId number) (SourceSpan 1 1) (emptyResolvedNodeFacts (NamedSourceUnit (nominalModulePath ("Draft" :| []))))
      expression =
        EIf
          (node 1)
          (ELit (node 2) (LBool True))
          (ETuple (node 3) [EList (node 4) [ELit (node 5) (LInt 1)], EIf (node 11) (ELit (node 12) (LBool True)) (ELit (node 6) (LBool True)) (ELit (node 13) (LBool False))])
          (ETuple (node 7) [EList (node 8) [ELit (node 9) (LInt 2)], ELit (node 10) (LBool False)])
      inputs = InferenceInputs Nothing defaultWarningSettings Set.empty Map.empty Map.empty Map.empty emptyScopeCapabilityFacts Set.empty
      (checked, state, _) = inferExpressionWork inputs expression
      erased = state {inferOutput = inferOutput initialInferState}
  expected <- either (fail . show) pure (finalizeCheckedExpression state checked)
  actual <- either (fail . show) pure (finalizeCheckedExpression erased checked)
  assertEqual "owned checked subtree survives output erasure" expected actual
  mapM_
    (assertOwned inputs)
    [ "(\\(x) -> x) (__kernel_add 1 2)",
      "case (1, [2]) { | (item, [other]) | (other, [item]) if __kernel_greaterThan item 0 -> __kernel_add item other | _ -> 0 }"
    ]
  mapM_
    (assertOwnedBlock inputs)
    [ "identity :: a -> a. identity = \\(item) -> item. first = identity @Int 1. identity = True. (first, identity).",
      "data Box a = Box a. class Equatable(a) { equals :: a -> a -> Bool. }. impl Equatable(Int) { equals = __kernel_equals. }. result = case Box 1 { | Box item -> Equatable::equals @Int item 1 }. result.",
      "left = \\(item) -> if True then item else right item. between = left 1. right = \\(item) -> left item. (between, right True)."
    ]
  where
    assertOwned inputs source = do
      resolved <- resolveFixtureProgram (Map.singleton "src/App/Main.jz" ("module App::Main { " <> source <> ". }"))
      let entry = NonEmpty.last (coreProgramModules resolved)
      case coreModuleStatements entry of
        [SExpr _ expression] -> assertDraft inputs expression
        statements -> fail ("unexpected ownership fixture: " <> show statements)
    assertOwnedBlock inputs source = do
      resolved <- resolveFixtureProgram (Map.singleton "src/App/Main.jz" ("module App::Main { " <> source <> " }"))
      assertDraft inputs (coreModuleExpr (NonEmpty.last (coreProgramModules resolved)))
    assertDraft inputs expression = do
      let (checked, state, _) = inferExpressionWork inputs expression
      owned <- either (fail . show) pure (finalizeCheckedExpression state checked)
      independent <- either (fail . show) pure (finalizeCheckedExpression (state {inferOutput = inferOutput initialInferState}) checked)
      assertEqual "checker retains each child and its decisions" owned independent
      assertExprFacts owned

testAnalyzedFactInvariantFailures :: IO ()
testAnalyzedFactInvariantFailures = do
  let modulePath = nominalModulePath ("Fact" :| [])
      owner = NamedSourceUnit modulePath
      node :: Int -> CoreNode 'Resolved 'ExpressionSort
      node number = CoreNode (CoreNodeId number) (SourceSpan 1 1) (emptyResolvedNodeFacts owner)
      expression = ELit (node 41) (LInt 1)
      unchecked = CheckedExpr Nothing (ELit <$> draftExpressionNode Nothing expression <*> pure (LInt 1))
  assertEqual "an incomplete checked node fails finalization" (Left (MissingExpressionFacts (CoreNodeId 41) :| [])) (finalizeCheckedExpression initialInferState unchecked)
  let unknown = BuiltinName (mkIdentifier "unknown")
      unresolvedNode = (node 42) {coreNodeFacts = (emptyResolvedNodeFacts owner) {resolvedNodeReference = Just (UnresolvedReference unknown)}}
      unresolved = EVar unresolvedNode unknown
      checkedUnresolved = CheckedExpr (Just SemanticInt) (EVar <$> draftExpressionNode (Just SemanticInt) unresolved <*> pure unknown)
  assertEqual "an unresolved reference cannot become analyzed" (Left (UnresolvedExpressionReference (CoreNodeId 42) unknown :| [])) (finalizeCheckedExpression initialInferState checkedUnresolved)
  let missingReference = EVar (node 43) unknown
      missingBinder = ELambda (node 44) unknown expression
      missingScope = EBlock (node 45) []
      finalizeNode value = finalizeCheckedExpression initialInferState (CheckedExpr (Just SemanticInt) (ELit <$> draftExpressionNode (Just SemanticInt) value <*> pure (LInt 1)))
  assertEqual "a missing reference cannot become analyzed" (Left (MissingExpressionFacts (CoreNodeId 43) :| [])) (finalizeNode missingReference)
  assertEqual "a missing lambda binder cannot become analyzed" (Left (MissingExpressionFacts (CoreNodeId 44) :| [])) (finalizeNode missingBinder)
  assertEqual "a missing lexical scope cannot become analyzed" (Left (MissingScopeFacts (CoreNodeId 45) :| [])) (finalizeNode missingScope)

  let statementId = CoreNodeId 51
      binder = CoreBinderId (owner, statementId)
      name = BuiltinName (mkIdentifier "value")
      statementNode = CoreNode statementId (SourceSpan 1 1) ((emptyResolvedNodeFacts owner) {resolvedNodeBinder = Just binder})
      statement = SLet statementNode name expression
      blockNode = (node 50) {coreNodeFacts = (emptyResolvedNodeFacts owner) {resolvedNodeScope = Just (ResolvedScopeFacts (Map.singleton 0 name) (Map.singleton 0 binder) Map.empty Map.empty Set.empty Set.empty)}}
      block = EBlock blockNode [statement]
      checkedValue = CheckedExpr (Just SemanticInt) (ELit <$> draftExpressionNode (Just SemanticInt) expression <*> pure (LInt 1))
      finalizeBinding binding = finalizeCheckedExpression initialInferState (CheckedExpr (Just SemanticInt) (EBlock <$> draftExpressionNode (Just SemanticInt) block <*> sequenceA [SLet <$> draftStatementNode statementNode (Just binding) (ValueDeclaration name) <*> pure name <*> checkedExprTree checkedValue]))
      aliasBinding = BuiltinAliasTypeBinding BuiltinToInt8
  assertEqual "an unprojected binding cannot silently lose its scheme" (Left (MissingStatementScheme statementId binder :| [])) (finalizeBinding aliasBinding)

  let variable = InferenceVariable 0
      scheme =
        SemanticScheme
          { schemeQuantifiedVariables = quantifiedVariablesFromPreferred [variable] (Set.singleton variable),
            schemeClassConstraints = [],
            schemePrimitiveConstraints = [TypeSchemeNumericConstraint (IntegralLiteralNumericConstraint (IntegerLiteralRange 1 1)) (SemanticVariable variable)],
            schemeResultType = SemanticVariable variable
          }
  case finalizeBinding (SchemeTypeBinding scheme) of
    Right (EBlock _ [SLet (CoreNode _ _ facts) _ _]) ->
      assertEqual "generalized schemes preserve integral literal ranges" True (any (schemeHasLiteralRange . snd) (statementBinding facts))
    result -> fail ("failed to finalize literal-range scheme: " <> show result)

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
      SClass _ _ _ methods _ _ ->
        [nodeIdentity node | ClassMethodSignature node _ _ <- methods]
      SImpl _ _ _ methods _ ->
        foldMap (\(ImplMethod node _ body) -> nodeIdentity node : exprNodeIdentities body) methods
      SExpr _ value -> exprNodeIdentities value
      _ -> []

assertExprFacts :: Expr 'Analyzed -> IO ()
assertExprFacts expression = do
  assertExpressionNodeFacts (exprNode expression)
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
assertExpressionNodeFacts (CoreNode _ _ facts) =
  mapM_ (assertEqual "result representation is concrete" True . Foldable.null) (expressionResultRepresentation facts)

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
        SImpl _ _ _ methods _ -> foldMap (\(ImplMethod _ _ body) -> expressionEvidenceInventory body) methods
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
        SImpl _ _ _ methods _ -> foldMap (\(ImplMethod _ _ body) -> expressionInstantiationInventory body) methods
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
          SClass _ _ _ methods _ _ -> foldMap (\(ClassMethodSignature node _ _) -> nodeBinders node) methods
          SImpl _ _ _ methods _ -> foldMap (\(ImplMethod node _ _) -> nodeBinders node) methods
          _ -> []
    nodeBinders (CoreNode _ _ facts) = foldMap (\(binder, _) -> [binder]) (statementBinding facts)

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
          SClass _ _ _ methods _ _ -> foldMap (\(ClassMethodSignature node _ _) -> nodeSchemes node) methods
          SImpl _ _ _ methods _ -> foldMap (\(ImplMethod node _ _) -> nodeSchemes node) methods
          _ -> []
    nodeSchemes (CoreNode _ _ facts) = foldMap (\(_, scheme) -> [scheme]) (statementBinding facts)

schemeHasLiteralRange :: AnalyzedScheme -> Bool
schemeHasLiteralRange scheme =
  any isLiteralRange (analyzedSchemePrimitiveConstraints scheme)
  where
    isLiteralRange constraint =
      case constraint of
        AnalyzedNumericPrimitiveConstraint (AnalyzedIntegralLiteralNumericConstraint 1 1) _ -> True
        _ -> False

expectedEvidenceIdentities :: CoreProgram 'Resolved -> [(CapabilityId, ImplId, MethodId)]
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
      MethodId (implementationId, mkIdentifier (identifierText methodName))
    )
  | coreModule <- NonEmpty.toList (coreProgramModules program),
    statement <- moduleStatements coreModule,
    SImpl implementationNode capabilityName [_] methods _ <- [statement],
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
        SClass _ name parameters methods _ _ -> do
          assertEqual "capability declaration fact" (CapabilityDeclaration name parameters) (statementDeclarationFact facts)
          mapM_ assertClassMethodFacts methods
        SImpl _ name _ methods _ -> do
          case statementDeclarationFact facts of
            ImplementationDeclaration factName -> assertEqual "implementation declaration identity" name factName
            other -> fail ("missing checked implementation declaration: " <> show other)
          mapM_ assertImplMethodFacts methods
        SModule _ path -> case statementDeclarationFact facts of
          ModuleDeclaration target -> assertEqual "module declaration fact" path (NonEmpty.toList (modulePathTextSegments target))
          _ -> fail "missing module declaration fact"
        SImport _ path _ _ -> case statementDeclarationFact facts of
          ImportDeclaration target -> assertEqual "import declaration fact" path (NonEmpty.toList (modulePathTextSegments target))
          _ -> fail "missing import declaration fact"
        SExpr _ value -> assertEqual "expression declaration fact" ExpressionDeclaration (statementDeclarationFact facts) >> assertExprFacts value
  case statement of
    SLet _ _ value -> assertExprFacts value
    _ -> pure ()
  where
    assertBindingStatement expected facts = do
      assertEqual "statement declaration identity" expected (statementDeclarationFact facts)
      case statementBinding facts of
        Just (binder, _) -> assertEqual "statement binding agrees with resolution" (Just binder) (resolvedNodeBinder (statementResolution facts))
        Nothing -> fail "statement is missing its analyzed binding"
    assertConstructorFacts (DataConstructor (CoreNode _ _ facts) name _) = assertBindingStatement (ValueDeclaration name) facts
    assertClassMethodFacts (ClassMethodSignature (CoreNode _ _ facts) name _) =
      case statementDeclarationFact facts of
        MethodDeclaration factName -> assertEqual "class method declaration identity" name factName
        other -> fail ("missing checked method declaration: " <> show other)
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
    SClass node _ _ _ _ _ -> node
    SImpl node _ _ _ _ -> node
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
        result = identity @Int (case Box 1 { | Box item -> if Equatable::equals item 1 then item else 0 }).
        result.
        }
        """
      ),
      ( "src/Lib/Facts.jz",
        """
        module Lib::Facts (identity, countdown, increment, type Box(Box), Equatable) {
        identity :: a -> a.
        identity = \\(item) -> item.
        countdown :: Int -> Int.
        countdown = \\(number) -> if __kernel_equals number 0 then 0 else countdown (__kernel_subtract number 1).
        increment = \\(number) -> __kernel_add number 1.
        data Box a = Box a.
        class Equatable(a) { equals :: a -> a -> Bool. }.
        impl Equatable(Int) { equals = __kernel_equals. }.
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
      case Map.lookup (resolvedImportedName (coreModulePath boxModule) TypeNamespace (mkIdentifier "Box")) (interfaceDataTypes (analyzedInterface boxModule)) of
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
      SClass node name parameters methods prerequisites defaults ->
        SClass node name parameters [ClassMethodSignature child methodName (SignatureType TypeBool) | ClassMethodSignature child methodName _ <- methods] prerequisites defaults
      SImpl node name _ methods prerequisites -> SImpl node name [] methods prerequisites
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
  assertEqual "two lexical identity definitions" 2 (length identityBinderIds)
  assertEqual
    "explicit applications reference their lexical definition-node binders"
    (Set.fromList [(identityBinderIds !! 0, SemanticInt), (identityBinderIds !! 1, SemanticBool)])
    (Set.fromList instantiatedBinderTypes)
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
          Just (binder, _) <- [statementBinding facts]
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
          [binder | identifierText name == "identity", Just (binder, _) <- [statementBinding facts]]
            <> identityDefinitionBinderIds value
        SImpl _ _ _ methods _ -> foldMap (\(ImplMethod _ _ body) -> identityDefinitionBinderIds body) methods
        SExpr _ value -> identityDefinitionBinderIds value
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
          | identifierText name == expectedName -> foldMap (\(_, scheme) -> [scheme]) (statementBinding facts)
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

testAnalyzedModulePublishesOnlyPublicDeclarations :: IO ()
testAnalyzedModulePublishesOnlyPublicDeclarations = do
  (_, analyzed) <- analyzeFixtureProgram explicitExportSources
  case lookupCoreModule (nominalModulePath ("Lib" :| ["Value"])) analyzed of
    Nothing -> fail "missing analyzed Lib::Value module"
    Just valueModule -> do
      assertEqual
        "public analyzed interface"
        (Set.singleton (ModuleExport ValueNamespace "answer"))
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
        Just runtimeModule -> do
          let exports = Map.keysSet (runtimeModuleExports runtimeModule)
              publicExport RuntimeImplementationMethodExport {} = False
              publicExport RuntimeDefaultMethodExport {} = False
              publicExport _ = True
          assertEqual
            "public class includes its ordinary method, without private method names"
            (Set.singleton (RuntimeBindingExport (ModuleExport ValueNamespace "equals")))
            (Set.filter publicExport exports)
          assertEqual
            "both implementation cells survive name selection"
            2
            (length [() | RuntimeImplementationMethodExport {} <- Set.toList exports])

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
        helper = \\(x) -> __kernel_add x 1.
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
        import Lib::Facts (Equatable).
        Equatable::equals 1 1.
        }
        """
      ),
      ( "src/Lib/Facts.jz",
        """
        module Lib::Facts (Equatable) {
        class Equatable(a) {
        equals :: a -> a -> Bool.
        }.
        class Hidden(a) {
        secret :: a -> Bool.
        }.
        impl Equatable(Int) {
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
          binder namespace = interfaceBindingReference <$> Map.lookup (ModuleExport namespace "Just") bindings
      assertEqual "analyzed shadowed export identities" expectedExports (Map.keysSet bindings)
      case coreModuleStatements maybeModule of
        [SData _ _ _ [DataConstructor constructorNode _ _], SLet valueNode _ _] -> do
          assertEqual
            "constructor interface retains its declaration ID"
            (LexicalReference <$> resolvedNodeBinder (statementResolution (coreNodeFacts constructorNode)))
            (binder ConstructorNamespace)
          assertEqual
            "value interface retains its distinct declaration ID"
            (LexicalReference <$> resolvedNodeBinder (statementResolution (coreNodeFacts valueNode)))
            (binder ValueNamespace)
        _ -> fail "unexpected constructor/value declaration fixture"
      case lookupCoreModule (nominalModulePath ("App" :| ["Main"])) analyzed of
        Just entry
          | [SExpr _ (EVar node _)] <- coreModuleStatements entry ->
              assertEqual
                "imported use retains the interface declaration ID"
                (binder ValueNamespace)
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
                          RuntimeImplementationMethodExport {} -> False
                          RuntimeDefaultMethodExport {} -> False
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
          "public grouped analyzed interface"
          (Set.singleton (ModuleExport ConstructorNamespace "First"))
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

testScopeStorageDefinitionSites :: IO ()
testScopeStorageDefinitionSites = do
  let source =
        Text.unlines
          [ "offset = 1.",
            "first = \\(n) -> if __kernel_equals n 0 then offset else second (__kernel_subtract n 1).",
            "offset = 9.",
            "second = \\(n) -> if __kernel_equals n 0 then offset else first (__kernel_subtract n 1).",
            "(first 1, second 1)."
          ]
      hostSource = "if False then __kernel_writeStdoutRaw! \"unused\" else (True, \"\", \"\", \"\"). " <> source
  calls <- newIORef []
  pureResult <- runSourceWithPrelude defaultWarningSettings Nothing source
  hostResult <- runSourceWithPreludeAndHost (recordingHost calls) defaultWarningSettings Nothing hostSource
  mapM_
    ( \result -> do
        assertEqual "recursive definition-site compile errors" [] (runCompileErrors result)
        assertEqual "recursive definition-site runtime errors" [] (runRuntimeErrors result)
        assertEqual "recursive definition-site output" (Just "(9, 1)") (runOutput result)
    )
    [pureResult, hostResult]
  assertEqual "unselected host branch" [] =<< readIORef calls

testScopeStorageInvocationIdentity :: IO ()
testScopeStorageInvocationIdentity = do
  calls <- newIORef []
  result <-
    runSourceWithPreludeAndHost
      (recordingHost calls)
      defaultWarningSettings
      Nothing
      "emit! = \\(text) -> { receipt! = __kernel_writeStdoutRaw! text. receipt!. receipt!. }. emit! \"first\". emit! \"second\"."
  assertEqual "invocation compile errors" [] (runCompileErrors result)
  assertEqual "invocation runtime errors" [] (runRuntimeErrors result)
  assertEqual "one effect per distinct invocation" ["first", "second"] =<< readIORef calls
  assertEqual "invocation result" (Just "(True, \"\", \"\", \"\")") (runOutput result)

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
          runProjectionFixture disabledRuntimeHost "module App::Main { __kernel_divide 1 0. }",
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
      ("src/Lib/Value.jz", "module Lib::Value { result = 1. __kernel_divide 1 0. }")
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
          ("src/Lib/Value.jz", "module Lib::Value { result = 1. __kernel_divide 1 0. }")
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
