{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.Support
  ( assertCompleteProduction,
    assertUnboundLater,
    assertUnboundName,
    fixtureByName,
    inferFixture,
    produceFixture,
    produceFixtureWithTrace,
    produceResolvedFixture,
    producerEdgeFixture,
    rejectedManifestExpectedStatuses,
    rejectedManifestFailureKinds,
    resolveFixtureModule,
    statusFailureKinds,
  )
where

import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures
import Jazz.Compiler.DiagnosticCatalog (diagnosticCodeText)
import Jazz.Compiler.Diagnostics
  ( diagnosticCode,
    diagnosticSubject,
    isErrorDiagnostic,
  )
import Jazz.Compiler.LoweredIR.Lower
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.ModuleGraph (CoreModule (..), ResolvedModule (..))
import Jazz.Compiler.TypeInference hiding (InferenceResult (..))
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
import Jazz.TestHarness (assertEqual, failTest)

assertCompleteProduction :: Text -> Fixture -> IO ()
assertCompleteProduction label fixture = do
  ordinary <- inferFixture fixture
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual (label <> " repeatable production") firstRun secondRun
  assertEqual
    (label <> " inference compatibility")
    ordinary
    (typedCoreProductionInferenceResult firstRun)
  assertEqual
    (label <> " diagnostics")
    []
    (filter isErrorDiagnostic (inferredDiagnostics (typedCoreProductionInferenceResult firstRun)))
  case typedCoreProductionStatus firstRun of
    TypedCoreProductionSucceeded programValue -> do
      assertEqual (label <> " typed-core validation") [] (validateTypedProgram programValue)
      case lowerTypedCoreExpressionDirectCall programValue of
        LoweredIRSucceeded loweredProgram ->
          assertEqual (label <> " lowered-IR validation") [] (validateLoweredProgram loweredProgram)
        _ -> failTest (label <> " did not lower successfully")
    status ->
      failTest
        ( label
            <> " did not produce typed core: "
            <> Text.pack (show status)
        )

assertUnboundName :: Text -> Text -> InferenceResult -> IO ()
assertUnboundName label name inferenceResult =
  assertEqual
    (label <> " reports " <> name <> " as unbound")
    True
    ( ("E1001", Just name)
        `elem` [ (diagnosticCodeText (diagnosticCode diagnostic), diagnosticSubject diagnostic)
               | diagnostic <- inferredDiagnostics inferenceResult,
                 isErrorDiagnostic diagnostic
               ]
    )

assertUnboundLater :: Text -> InferenceResult -> IO ()
assertUnboundLater label = assertUnboundName label "later"

rejectedManifestExpectedStatuses :: [(Text, TypedCoreProductionStatus)]
rejectedManifestExpectedStatuses =
  [ ("source-diagnostic", TypedCoreProductionBlockedByDiagnostics),
    ( "invalid-portable-source-path",
      unsupported [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreInvalidPortableSourcePath TypedCoreNoFailureDetail]
    ),
    ( "resolved-import",
      unsupported [TypedCoreProductionFailure (TypedCoreProductionModulePath ["App", "Main"]) TypedCoreResolvedImportsUnsupported TypedCoreNoFailureDetail]
    ),
    ( "ambient-prelude-input",
      unsupported [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreAmbientPreludeInputUnsupported TypedCoreNoFailureDetail]
    ),
    ( "text-value",
      unsupported
        [expressionFailure 1 [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]
    ),
    ("list-value", unsupported [expressionFailure 0 [] TypedCoreStructuredValueUnsupported TypedCoreListValueDetail]),
    ("local-block-binding", unsupported [expressionFailure 0 [] TypedCoreNestedBlockUnsupported TypedCoreLocalBlockDetail]),
    ( "oversaturated-direct-call",
      unsupported
        [expressionFailure 1 [0, 0] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail]
    ),
    ( "later-capture-mutual-recursion",
      unsupported
        [statementFailure 4 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "right")]
    ),
    ( "transitive-later-capture-mutual-recursion",
      unsupported
        [statementFailure 6 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "right")]
    ),
    ( "interleaved-rebound-capture-mutual-recursion",
      unsupported
        [statementFailure 5 TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail "right")]
    ),
    ( "polymorphic-or-evidence-function",
      unsupported
        [ expressionFailure 0 [] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail,
          statementFailure 1 TypedCoreNonMonomorphicFunctionUnsupported (TypedCoreNameDetail "identity"),
          expressionFailure 1 [] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail,
          expressionFailure 1 [0] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail,
          expressionFailure 2 [] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail
        ]
    ),
    ( "imported-direct-call",
      unsupported [TypedCoreProductionFailure TypedCoreProductionInputPath TypedCoreImportedInputsUnsupported TypedCoreNoFailureDetail]
    ),
    ( "user-defined-operator-call",
      unsupported
        [ statementFailure 1 TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail,
          expressionFailure 2 [] TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail
        ]
    )
  ]
  where
    unsupported = TypedCoreProductionUnsupported
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

rejectedManifestFailureKinds :: [(Text, [TypedCoreProductionFailureKind])]
rejectedManifestFailureKinds =
  [ (name, statusFailureKinds status)
  | (name, status) <- rejectedManifestExpectedStatuses
  ]

statusFailureKinds :: TypedCoreProductionStatus -> [TypedCoreProductionFailureKind]
statusFailureKinds status =
  case status of
    TypedCoreProductionBlockedByDiagnostics -> []
    TypedCoreProductionUnsupported failures ->
      [ kind
      | TypedCoreProductionFailure _ kind _ <- failures
      ]
    TypedCoreProductionInvariantFailures _ -> []
    TypedCoreProductionSucceeded _ -> []

resolveFixtureModule :: Fixture -> IO ResolvedModule
resolveFixtureModule fixture = do
  result <- resolveFixture fixture
  case result of
    Left failures ->
      failTest
        ( fixtureName fixture
            <> " did not resolve through the module resolver: "
            <> Text.pack (show failures)
        )
    Right resolvedModule -> pure resolvedModule

inferFixture :: Fixture -> IO InferenceResult
inferFixture fixture = do
  resolvedModule <- resolveFixtureModule fixture
  inferExpressionWithInputs (fixtureInputs fixture) (coreModuleExpr (resolvedModuleCore resolvedModule))

produceFixture :: Fixture -> IO TypedCoreProductionResult
produceFixture fixture = do
  resolvedModule <- resolveFixtureModule fixture
  produceResolvedFixture fixture resolvedModule

produceFixtureWithTrace :: Fixture -> IO (TypedCoreProductionResult, [FilePath])
produceFixtureWithTrace fixture = do
  lookupPaths <- newIORef []
  resolvedResult <- resolveFixtureWithLookup fixture $ \path -> do
    modifyIORef' lookupPaths (<> [path])
    pure (Map.lookup path (fixtureSourceFiles fixture))
  resolvedModule <-
    case resolvedResult of
      Left _ -> failTest (fixtureName fixture <> " did not resolve through the module resolver")
      Right value -> pure value
  productionResult <- produceResolvedFixture fixture resolvedModule
  lookupPathsValue <- readIORef lookupPaths
  pure (productionResult, lookupPathsValue)

produceResolvedFixture :: Fixture -> ResolvedModule -> IO TypedCoreProductionResult
produceResolvedFixture fixture resolvedModule =
  inferResolvedModuleTypedCoreExpressionDirectCall
    (fixtureInputs fixture)
    (fixtureSourcePath fixture)
    resolvedModule

producerEdgeFixture :: Text -> Fixture
producerEdgeFixture name =
  case lookup name producerEdgeFixtures of
    Just fixture -> fixture
    Nothing -> error ("producer edge fixture is missing: " <> Text.unpack name)

fixtureByName :: Text -> Fixture
fixtureByName name =
  case filter ((== name) . fixtureName) fixtures of
    [fixture] -> fixture
    [] -> error ("fixture is missing: " <> Text.unpack name)
    _ -> error ("fixture name is ambiguous: " <> Text.unpack name)
