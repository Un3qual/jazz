{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Semantics.Runtime.HostIOTests
  ( hostIOTests,
  )
where

import Control.Exception (finally)
import Control.Monad.Trans.State.Strict
  ( State,
    modify,
    runState,
  )
import qualified Data.ByteString as ByteString
import Data.Either (isRight)
import Data.Functor.Identity (Identity (..))
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (Analyzed),
    Expr,
    Literal (..),
  )
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (..))
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.Driver
  ( runCompileErrors,
    runOutput,
    runRuntimeErrors,
    runSourceWithPreludeAndHost,
  )
import Jazz.Compiler.Name (UnresolvedName, qualifiedName)
import Jazz.Compiler.RecursiveBindings (emptyLambdaCaptureHints)
import Jazz.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeAnnotation (..),
    RuntimeValue (..),
    ScopeResult (..),
    evaluateModuleScopeWithHost,
    evaluateModuleScopeWithRequiredEvaluationHost,
    evaluateModuleScopeWithRequiredHost,
    evaluateRuntimeExpr,
    evaluateRuntimeExprWithHost,
    prependRuntimeExplicitResultHint,
    renderRuntimeValue,
    runRuntimeHostEvaluation,
    runtimeValueExactlyMatchesConstraint,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeCallableIdentity (ClosureCallable),
  )
import Jazz.Compiler.Runtime.Types (RuntimeClosure (..))
import Jazz.Compiler.RuntimeHost
  ( HostIOCategory (..),
    HostIOFailure (..),
    RuntimeHost (..),
    RuntimeHostExit (..),
    hostIOCategoryToken,
    hostIOFailureMessage,
    productionRuntimeHost,
  )
import Jazz.Compiler.Semantics.Runtime.Fixtures
import Jazz.Compiler.Semantics.Runtime.Shared (assertRuntimeBool)
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    SemanticType (..),
    SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftDiagnosticContains,
    failTest,
  )
import System.Directory
  ( getTemporaryDirectory,
    removeFile,
  )
import System.Environment (getArgs)
import System.IO
  ( hClose,
    openBinaryTempFile,
  )
import System.Timeout (timeout)

hostIOTests :: [NamedTest]
hostIOTests =
  [ ("host-aware evaluator preserves pure expressions", testHostAwareEvaluatorPreservesPureExpressions),
    ("host-backed tail recursion is stack safe and preserves effect order", testHostTailRecursionIsStackSafe),
    ("host intrinsics return raw values and preserve call order", testHostIntrinsicsReturnRawValues),
    ("host failures normalize every category", testHostFailuresNormalizeEveryCategory),
    ("host effects execute at selected expression depth", testHostEffectsExecuteAtSelectedExpressionDepth),
    ("host-dependent function selectors use the injected host", testHostDependentFunctionSelector),
    ("host scopes preserve mutually recursive functions", testHostScopePreservesMutualRecursion),
    ("host scopes preserve hostful recursive peers", testHostScopePreservesHostfulRecursivePeers),
    ("host scopes evaluate impl method selectors with the injected host", testHostImplMethodSelector),
    ("host scopes preserve binding signature hints", testHostScopePreservesBindingSignatureHints),
    ("host dependency scopes keep unused bindings lazy", testHostDependencyScopeKeepsUnusedBindingLazy),
    ("host dependency bindings are shared when forced", testHostDependencyBindingIsShared),
    ("host map callbacks preserve the active host cache and effect order", testHostMapCallbackPreservesActiveHostCacheAndEffectOrder),
    ("public host scopes keep imported deferred cells on the active host", testPublicHostScopeKeepsImportedDeferredCellOnActiveHost),
    ("host dependency scopes keep deferred cells on the active host", testHostDependencyScopeKeepsDeferredCellsOnActiveHost),
    ("host dependency bindings retain their analyzed runtime plans", testHostDependencyBindingRetainsRuntimePlan),
    ("stacked result obligations preserve recursive unwind order", testStackedResultObligationsPreserveRecursiveUnwindOrder),
    ("host binding cache separates dynamic scope invocations", testHostBindingCacheSeparatesDynamicScopeInvocations),
    ("host scopes force zero-argument impl methods", testHostZeroArgumentImplMethod),
    ("nullary evidence selection preserves host method caching", testNullaryEvidencePreservesHostMethodCaching),
    ("direct runtime wrappers normalize disabled host calls", testDirectRuntimeWrapperUsesDisabledHost),
    ("direct runtime wrappers reject disabled host exits", testDirectRuntimeWrapperRejectsDisabledExit),
    ("exit rejects statuses outside the portable range", testExitRejectsInvalidStatus),
    ("standalone source execution injects its runtime host", testStandaloneSourceInjectsRuntimeHost),
    ("production host round trips multibyte UTF-8", testProductionHostRoundTripsUtf8),
    ("production host classifies missing files", testProductionHostClassifiesMissingFile),
    ("production host rejects invalid UTF-8", testProductionHostRejectsInvalidUtf8),
    ("production host exposes process arguments", testProductionHostExposesArguments)
  ]

testHostTailRecursionIsStackSafe :: IO ()
testHostTailRecursionIsStackSafe = do
  callsRef <- newIORef []
  let isZero = expressionBinary "==" (expressionVariable "remaining") (expressionLiteral (LInt 0))
      decrement =
        expressionApply
          (expressionVariable "countDown!")
          (expressionBinary "-" (expressionVariable "remaining") (expressionLiteral (LInt 1)))
      expression =
        expressionBlock
          [ statementLet
              "countDown!"
              (SourceSpan 1 1)
              (expressionLambda "remaining" (expressionIf isZero (expressionLiteral (LInt 0)) decrement)),
            statementExpression
              (SourceSpan 2 1)
              (hostCall "__kernel_writeStdoutRaw!" [expressionLiteral (LText "before")]),
            statementExpression
              (SourceSpan 3 1)
              (expressionApply (expressionVariable "countDown!") (expressionLiteral (LInt 20000)))
          ]
  maybeOutcome <-
    timeout
      30000000
      (evaluateRuntimeExprWithHost (recordingIOHost callsRef) expression)
  case maybeOutcome of
    Nothing -> failTest "20,000-call host-path tail recursion timed out"
    Just result -> do
      calls <- readIORef callsRef
      assertEqual
        "host-path tail result"
        (Right (Just "0"))
        (fmap (fmap renderRuntimeValue) result)
      assertEqual "host-path effects execute once" [WriteStdoutCall "before"] calls

testHostAwareEvaluatorPreservesPureExpressions :: IO ()
testHostAwareEvaluatorPreservesPureExpressions = do
  mapM_ assertPreserved expressions
  where
    expressions =
      [ expressionBinary "+" (expressionLiteral (LInt 20)) (expressionLiteral (LInt 22)),
        expressionApply (expressionLambda "itemValue" (expressionBinary "+" (expressionVariable "itemValue") (expressionLiteral (LInt 2)))) (expressionLiteral (LInt 40)),
        expressionBlock
          [ statementLet "itemValue" (SourceSpan 1 1) (expressionLiteral (LInt 40)),
            statementExpression (SourceSpan 2 1) (expressionBinary "+" (expressionVariable "itemValue") (expressionLiteral (LInt 2)))
          ]
      ]

    assertPreserved expression = do
      let expected = evaluateRuntimeExpr expression
          actual = runIdentity (evaluateRuntimeExprWithHost deterministicHost expression)
      assertEqual
        "host-aware pure result"
        (fmap (fmap renderRuntimeValue) expected)
        (fmap (fmap renderRuntimeValue) actual)

deterministicHost :: RuntimeHost Identity
deterministicHost =
  RuntimeHost
    { runtimeHostReadText = \_ -> pure (Right "unused"),
      runtimeHostWriteText = \_ _ -> pure (Right ()),
      runtimeHostReadStdin = pure (Right "unused"),
      runtimeHostWriteStdout = \_ -> pure (Right ()),
      runtimeHostWriteStderr = \_ -> pure (Right ()),
      runtimeHostArguments = pure [],
      runtimeHostExit = \_ -> pure (Right RuntimeHostExitReturned)
    }

data HostCall
  = ReadTextCall Text
  | WriteTextCall Text Text
  | ReadStdinCall
  | WriteStdoutCall Text
  | WriteStderrCall Text
  | ArgumentsCall
  | ExitCall Integer
  deriving (Eq, Show)

testHostIntrinsicsReturnRawValues :: IO ()
testHostIntrinsicsReturnRawValues = do
  let expressions =
        [ hostCall "__kernel_readTextRaw!" [expressionLiteral (LText "source.jz")],
          hostCall "__kernel_writeTextRaw!" [expressionLiteral (LText "output.txt"), expressionLiteral (LText "Jazz")],
          hostCall "__kernel_readStdinRaw!" [expressionTuple []],
          hostCall "__kernel_writeStdoutRaw!" [expressionLiteral (LText "out")],
          hostCall "__kernel_writeStderrRaw!" [expressionLiteral (LText "err")],
          hostCall "__kernel_arguments!" [expressionTuple []],
          hostCall "__kernel_exit!" [expressionLiteral (LInt 7)]
        ]
      (results, calls) = runState (traverse (evaluateRuntimeExprWithHost statefulHost) expressions) []
  assertEqual
    "host intrinsic raw values"
    [ Right (Just "(True, \"file text\", \"\", \"\")"),
      Right (Just "(True, \"\", \"\", \"\")"),
      Right (Just "(True, \"stdin text\", \"\", \"\")"),
      Right (Just "(True, \"\", \"\", \"\")"),
      Right (Just "(True, \"\", \"\", \"\")"),
      Right (Just "[\"one\", \"two\"]"),
      Right (Just "()")
    ]
    (map (fmap (fmap renderRuntimeValue)) results)
  assertEqual
    "host call order"
    [ ReadTextCall "source.jz",
      WriteTextCall "output.txt" "Jazz",
      ReadStdinCall,
      WriteStdoutCall "out",
      WriteStderrCall "err",
      ArgumentsCall,
      ExitCall 7
    ]
    calls

testHostFailuresNormalizeEveryCategory :: IO ()
testHostFailuresNormalizeEveryCategory =
  mapM_ assertCategory allCategories
  where
    allCategories =
      [ HostNotFound,
        HostPermissionDenied,
        HostAlreadyExists,
        HostInvalidData,
        HostResourceExhausted,
        HostInterrupted,
        HostUnsupported,
        HostOther
      ]

    assertCategory category = do
      let host = deterministicHost {runtimeHostReadText = \_ -> pure (Left (HostIOFailure category "host-specific detail"))}
          expression = hostCall "__kernel_readTextRaw!" [expressionLiteral (LText "missing.jz")]
          actual = runIdentity (evaluateRuntimeExprWithHost host expression)
          expected = Right (Just (rawFailure category))
      assertEqual
        "normalized host failure category"
        (fmap (fmap renderRuntimeValue) expected)
        (fmap (fmap renderRuntimeValue) actual)

testHostEffectsExecuteAtSelectedExpressionDepth :: IO ()
testHostEffectsExecuteAtSelectedExpressionDepth = do
  let expressions =
        [ expressionApply
            (expressionLambda "itemValue" (hostCall "__kernel_writeStdoutRaw!" [expressionVariable "itemValue"]))
            (expressionLiteral (LText "closure")),
          expressionIf
            (expressionLiteral (LBool False))
            (hostCall "__kernel_writeStderrRaw!" [expressionLiteral (LText "skipped")])
            (hostCall "__kernel_writeStdoutRaw!" [expressionLiteral (LText "branch")]),
          expressionPatternCase
            (expressionLiteral (LBool True))
            [ caseArm (patternLiteral (LBool True)) Nothing (hostCall "__kernel_writeStderrRaw!" [expressionLiteral (LText "arm")]),
              caseArm patternWildcard Nothing (hostCall "__kernel_writeStderrRaw!" [expressionLiteral (LText "fallback")])
            ],
          expressionBlock
            [ statementExpression (SourceSpan 1 1) (hostCall "__kernel_writeStdoutRaw!" [expressionLiteral (LText "block")])
            ]
        ]
      (results, calls) = runState (traverse (evaluateRuntimeExprWithHost statefulHost) expressions) []
  assertEqual
    "nested effect results"
    (replicate 4 (Right (Just "(True, \"\", \"\", \"\")")))
    (map (fmap (fmap renderRuntimeValue)) results)
  assertEqual
    "only selected nested effects run"
    [ WriteStdoutCall "closure",
      WriteStdoutCall "branch",
      WriteStderrCall "arm",
      WriteStdoutCall "block"
    ]
    calls

testHostDependentFunctionSelector :: IO ()
testHostDependentFunctionSelector = do
  let selector =
        expressionBinary
          "=="
          (hostCall "__kernel_arguments!" [expressionTuple []])
          (expressionList [expressionLiteral (LText "one"), expressionLiteral (LText "two")])
      expression =
        expressionBlock
          [ statementLet
              "choose!"
              (SourceSpan 1 1)
              ( expressionIf
                  selector
                  (expressionLambda "ignored" (expressionLiteral (LInt 1)))
                  (expressionLambda "ignored" (expressionLiteral (LInt 2)))
              ),
            statementExpression (SourceSpan 2 1) (expressionApply (expressionVariable "choose!") (expressionTuple []))
          ]
      (result, calls) = runState (evaluateRuntimeExprWithHost statefulHost expression) []
  assertEqual "host-selected closure result" (Right (Just "1")) (fmap (fmap renderRuntimeValue) result)
  assertEqual "host selector call" [ArgumentsCall] calls

testHostScopePreservesMutualRecursion :: IO ()
testHostScopePreservesMutualRecursion = do
  let decrement name = expressionApply (expressionVariable name) (expressionBinary "-" (expressionVariable "itemValue") (expressionLiteral (LInt 1)))
      isZero = expressionBinary "==" (expressionVariable "itemValue") (expressionLiteral (LInt 0))
      expression =
        expressionBlock
          [ statementLet
              "even"
              (SourceSpan 1 1)
              (expressionLambda "itemValue" (expressionIf isZero (expressionLiteral (LBool True)) (decrement "odd"))),
            statementLet
              "odd"
              (SourceSpan 2 1)
              (expressionLambda "itemValue" (expressionIf isZero (expressionLiteral (LBool False)) (decrement "even"))),
            statementExpression (SourceSpan 3 1) (hostCall "__kernel_writeStdoutRaw!" [expressionLiteral (LText "once")]),
            statementExpression (SourceSpan 4 1) (expressionApply (expressionVariable "even") (expressionLiteral (LInt 4)))
          ]
      (result, calls) = runState (evaluateRuntimeExprWithHost statefulHost expression) []
  assertRuntimeBool "mutually recursive result" True result
  assertEqual "unrelated host call" [WriteStdoutCall "once"] calls

testHostScopePreservesHostfulRecursivePeers :: IO ()
testHostScopePreservesHostfulRecursivePeers = do
  let decrement name = expressionApply (expressionVariable name) (expressionBinary "-" (expressionVariable "itemValue") (expressionLiteral (LInt 1)))
      isZero = expressionBinary "==" (expressionVariable "itemValue") (expressionLiteral (LInt 0))
      expression =
        expressionBlock
          [ statementLet
              "even!"
              (SourceSpan 1 1)
              ( expressionLambda
                  "itemValue"
                  ( expressionIf
                      isZero
                      (expressionLiteral (LBool True))
                      ( expressionBlock
                          [ statementExpression (SourceSpan 2 1) (hostCall "__kernel_writeStdoutRaw!" [expressionLiteral (LText "even")]),
                            statementExpression (SourceSpan 3 1) (decrement "odd!")
                          ]
                      )
                  )
              ),
            statementLet
              "odd!"
              (SourceSpan 4 1)
              (expressionLambda "itemValue" (expressionIf isZero (expressionLiteral (LBool False)) (decrement "even!"))),
            statementExpression (SourceSpan 5 1) (expressionApply (expressionVariable "even!") (expressionLiteral (LInt 2)))
          ]
      (result, calls) = runState (evaluateRuntimeExprWithHost statefulHost expression) []
  assertRuntimeBool "hostful mutually recursive result" True result
  assertEqual "hostful recursive call" [WriteStdoutCall "even"] calls

testHostImplMethodSelector :: IO ()
testHostImplMethodSelector = do
  let selector =
        expressionBinary
          "=="
          (hostCall "__kernel_arguments!" [expressionTuple []])
          (expressionList [expressionLiteral (LText "one"), expressionLiteral (LText "two")])
      expression =
        expressionBlock
          [ statementClass
              (SourceSpan 1 1)
              "RuntimePick"
              ["a"]
              [ classMethodSignature
                  "pick"
                  (SourceSpan 2 1)
                  (ConstrainedSignature [] (TypeFunction (fixtureTypeVariable "a") TypeBool))
              ],
            statementImpl
              (SourceSpan 3 1)
              "RuntimePick"
              [TypeInt]
              [ implMethod
                  "pick"
                  (SourceSpan 4 1)
                  ( expressionIf
                      selector
                      (expressionLambda "ignored" (expressionLiteral (LBool True)))
                      (expressionLambda "ignored" (expressionLiteral (LBool False)))
                  )
              ],
            statementExpression
              (SourceSpan 5 1)
              (expressionApply (expressionVariable (qualifiedName "RuntimePick" "pick")) (expressionLiteral (LInt 1)))
          ]
      (result, calls) = runState (evaluateRuntimeExprWithHost statefulHost expression) []
  assertRuntimeBool "host-selected impl method result" True result
  assertEqual "host-selected impl method call" [ArgumentsCall] calls

testHostScopePreservesBindingSignatureHints :: IO ()
testHostScopePreservesBindingSignatureHints = do
  let expression =
        expressionBlock
          [ statementSignature "itemValue" (SourceSpan 1 1) (SignatureType (TypeNumeric NumericInt8)),
            statementLet
              "itemValue"
              (SourceSpan 2 1)
              (expressionConstrainedAs (TypeNumeric NumericInt8) (expressionLiteral (LInt 1))),
            statementExpression (SourceSpan 3 1) (hostCall "__kernel_writeStdoutRaw!" [expressionLiteral (LText "once")]),
            statementExpression (SourceSpan 4 1) (expressionVariable "itemValue")
          ]
      (result, calls) = runState (evaluateRuntimeExprWithHost statefulHost expression) []
  assertEqual "signature host call" [WriteStdoutCall "once"] calls
  case result of
    Right (Just itemValue) ->
      assertEqual
        "host scope keeps Int8 runtime hint"
        True
        (runtimeValueExactlyMatchesConstraint (SemanticNumeric NumericInt8) itemValue)
    _ -> assertEqual "host scope produces signed itemValue" True False

testHostDependencyScopeKeepsUnusedBindingLazy :: IO ()
testHostDependencyScopeKeepsUnusedBindingLazy = do
  let statements =
        [ statementLet
            "unused!"
            (SourceSpan 1 1)
            (hostCall "__kernel_writeStdoutRaw!" [expressionLiteral (LText "unused")])
        ]
      (result, calls) =
        runState
          ( evaluateModuleScopeWithRequiredHost
              statefulHost
              Nothing
              EvaluateDependencyModule
              ResolveKernelOnly
              Map.empty
              statements
          )
          []
  assertEqual "dependency scope result" True (isRight result)
  assertEqual "unused dependency host calls" [] calls

testHostDependencyBindingIsShared :: IO ()
testHostDependencyBindingIsShared = do
  let dependencyStatements =
        [ statementLet
            "token!"
            (SourceSpan 1 1)
            (hostCall "__kernel_readStdinRaw!" [expressionTuple []])
        ]
      entryStatements =
        [ statementExpression
            (SourceSpan 2 1)
            (expressionTuple [expressionVariable "token!", expressionVariable "token!"])
        ]
      action = do
        dependencyResult <-
          evaluateModuleScopeWithRequiredHost
            statefulHost
            (Just ["Dependency"])
            EvaluateDependencyModule
            ResolveKernelOnly
            Map.empty
            dependencyStatements
        case dependencyResult of
          Left diagnostic -> pure (Left diagnostic)
          Right dependencyScope ->
            evaluateModuleScopeWithRequiredHost
              statefulHost
              (Just ["Main"])
              EvaluateEntryModule
              ResolveKernelOnly
              (scopeResultEnvironment dependencyScope)
              entryStatements
      (result, calls) = runState action []
  assertEqual "shared dependency binding result" True (isRight result)
  assertEqual "shared dependency host call" [ReadStdinCall] calls

testHostMapCallbackPreservesActiveHostCacheAndEffectOrder :: IO ()
testHostMapCallbackPreservesActiveHostCacheAndEffectOrder = do
  let mapper =
        expressionLambda
          "label"
          ( expressionBlock
              [ statementExpression
                  (SourceSpan 2 1)
                  (hostCall "__kernel_writeStdoutRaw!" [expressionVariable "label"]),
                statementExpression (SourceSpan 3 1) (expressionVariable "token!")
              ]
          )
      dependencyStatements =
        [ statementLet
            "token!"
            (SourceSpan 1 1)
            (hostCall "__kernel_readStdinRaw!" [expressionTuple []])
        ]
      entryStatements =
        [ statementExpression
            (SourceSpan 4 1)
            ( expressionApply
                (expressionApply (expressionVariable "__kernel_map") mapper)
                (expressionList [expressionLiteral (LText "first"), expressionLiteral (LText "second")])
            )
        ]
      action =
        runRuntimeHostEvaluation statefulHost $ \evaluationHost -> do
          dependencyResult <-
            evaluateModuleScopeWithRequiredEvaluationHost
              evaluationHost
              (Just ["Dependency"])
              EvaluateDependencyModule
              ResolveKernelOnly
              Map.empty
              dependencyStatements
          case dependencyResult of
            Left diagnostic -> pure (Left diagnostic)
            Right dependencyScope ->
              evaluateModuleScopeWithRequiredEvaluationHost
                evaluationHost
                (Just ["Main"])
                EvaluateEntryModule
                ResolveKernelOnly
                (scopeResultEnvironment dependencyScope)
                entryStatements
      (result, calls) = runState action []
  case result of
    Right scopeResult ->
      assertEqual
        "host map callback returns the shared raw stdin result for each element"
        (Just "[(True, \"stdin text\", \"\", \"\"), (True, \"stdin text\", \"\", \"\")]")
        (fmap renderRuntimeValue (scopeResultValue scopeResult))
    Left _ -> assertEqual "host map callback evaluates" True False
  assertEqual
    "host map callback keeps effects ordered and caches the deferred stdin read"
    [WriteStdoutCall "first", ReadStdinCall, WriteStdoutCall "second"]
    calls

testPublicHostScopeKeepsImportedDeferredCellOnActiveHost :: IO ()
testPublicHostScopeKeepsImportedDeferredCellOnActiveHost = do
  let dependencyStatements =
        [ statementLet
            "token!"
            (SourceSpan 1 1)
            (hostCall "__kernel_readStdinRaw!" [expressionTuple []])
        ]
      entryStatements = [statementExpression (SourceSpan 2 1) (expressionVariable "token!")]
      action = do
        dependencyResult <-
          evaluateModuleScopeWithRequiredHost
            statefulHost
            (Just ["Dependency"])
            EvaluateDependencyModule
            ResolveKernelOnly
            Map.empty
            dependencyStatements
        case dependencyResult of
          Left diagnostic -> pure (Left diagnostic)
          Right dependencyScope ->
            evaluateModuleScopeWithHost
              statefulHost
              (Just ["Main"])
              EvaluateEntryModule
              ResolveKernelOnly
              (scopeResultEnvironment dependencyScope)
              entryStatements
      (result, calls) = runState action []
  case result of
    Right scopeResult ->
      assertEqual
        "public host scope imported binding result"
        (Just "(True, \"stdin text\", \"\", \"\")")
        (fmap renderRuntimeValue (scopeResultValue scopeResult))
    Left _ -> assertEqual "public host scope imported binding evaluates" True False
  assertEqual "public host scope imported binding call" [ReadStdinCall] calls

testHostDependencyScopeKeepsDeferredCellsOnActiveHost :: IO ()
testHostDependencyScopeKeepsDeferredCellsOnActiveHost = do
  let dependencyStatements =
        [ statementLet
            "token!"
            (SourceSpan 1 1)
            (hostCall "__kernel_readStdinRaw!" [expressionTuple []])
        ]
      entryStatements =
        [ statementLet
            "selected"
            (SourceSpan 2 1)
            (expressionIf (expressionLiteral (LBool True)) (expressionVariable "token!") (expressionVariable "peer")),
          statementLet
            "peer"
            (SourceSpan 3 1)
            (expressionVariable "selected"),
          statementExpression (SourceSpan 4 1) (expressionVariable "selected")
        ]
      action = do
        dependencyResult <-
          evaluateModuleScopeWithRequiredHost
            statefulHost
            (Just ["Dependency"])
            EvaluateDependencyModule
            ResolveKernelOnly
            Map.empty
            dependencyStatements
        case dependencyResult of
          Left diagnostic -> pure (Left diagnostic)
          Right dependencyScope ->
            evaluateModuleScopeWithRequiredHost
              statefulHost
              (Just ["Main"])
              EvaluateEntryModule
              ResolveKernelOnly
              (scopeResultEnvironment dependencyScope)
              entryStatements
      (result, calls) = runState action []
  case result of
    Right scopeResult ->
      assertEqual
        "mixed host/pure dependency result"
        (Just "(True, \"stdin text\", \"\", \"\")")
        (fmap renderRuntimeValue (scopeResultValue scopeResult))
    Left _ -> assertEqual "mixed host/pure dependency evaluation succeeds" True False
  assertEqual "mixed host/pure dependency call" [ReadStdinCall] calls

testStackedResultObligationsPreserveRecursiveUnwindOrder :: IO ()
testStackedResultObligationsPreserveRecursiveUnwindOrder = do
  let identityClosure =
        VClosure
          RuntimeClosure
            { runtimeClosureEnvironment = Map.empty,
              runtimeClosureEnvironmentMayReachHostCells = False,
              runtimeClosureLambdaCaptureHints = emptyLambdaCaptureHints,
              runtimeClosureParameter = fixtureValueName "itemValue",
              runtimeClosureBody = expressionVariable "itemValue",
              runtimeClosureTypeHint = Nothing,
              runtimeClosureModulePath = Nothing,
              runtimeClosureCallableIdentity = ClosureCallable "<test>" 1 "itemValue"
            }
      stackedFunction =
        VAnnotated
          (RuntimeTypeHint (SemanticFunction SemanticInt SemanticInt))
          (prependRuntimeExplicitResultHint (SemanticNumeric NumericUInt8) identityClosure)
      statements =
        [ statementExpression
            (SourceSpan 1 1)
            (expressionApply (expressionVariable "convert") (expressionLiteral (LInt 200)))
        ]
      (result, calls) =
        runState
          ( evaluateModuleScopeWithRequiredHost
              statefulHost
              Nothing
              EvaluateEntryModule
              ResolveKernelOnly
              (Map.singleton (fixtureValueName "convert") (Right stackedFunction))
              statements
          )
          []
  assertEqual "stacked result obligation host calls" [] calls
  case result of
    Right scopeResult ->
      case scopeResultValue scopeResult of
        Just itemValue -> do
          assertEqual
            "outer result hint applies after inner result hint"
            True
            (runtimeValueExactlyMatchesConstraint SemanticInt itemValue)
          assertEqual
            "inner result hint does not escape the outer result hint"
            False
            (runtimeValueExactlyMatchesConstraint (SemanticNumeric NumericUInt8) itemValue)
        Nothing -> assertEqual "stacked result obligations produce a itemValue" True False
    Left _ -> assertEqual "stacked result obligations evaluate" True False

testHostDependencyBindingRetainsRuntimePlan :: IO ()
testHostDependencyBindingRetainsRuntimePlan = do
  let typeArgumentSpan = SourceSpan 2 18
      dependencyStatements =
        [ statementLet "identity" (SourceSpan 1 1) (expressionLambda "itemValue" (expressionVariable "itemValue")),
          statementLet
            "token!"
            (SourceSpan 2 1)
            ( expressionApply
                (expressionTypeApplication (expressionVariable "identity") typeArgumentSpan (TypeNumeric NumericUInt8))
                (expressionLiteral (LInt 1))
            )
        ]
      entryStatements = [statementExpression (SourceSpan 3 1) (expressionVariable "token!")]
      action = do
        dependencyResult <-
          evaluateModuleScopeWithRequiredHost
            statefulHost
            (Just ["Dependency"])
            EvaluateDependencyModule
            ResolveKernelOnly
            Map.empty
            dependencyStatements
        case dependencyResult of
          Left diagnostic -> pure (Left diagnostic)
          Right dependencyScope ->
            evaluateModuleScopeWithRequiredHost
              statefulHost
              (Just ["Main"])
              EvaluateEntryModule
              ResolveKernelOnly
              (scopeResultEnvironment dependencyScope)
              entryStatements
      (result, calls) = runState action []
  assertEqual "planned dependency host calls" [] calls
  case result of
    Right scopeResult ->
      case scopeResultValue scopeResult of
        Just itemValue ->
          assertEqual
            "dependency keeps UInt8 runtime plan"
            True
            (runtimeValueExactlyMatchesConstraint (SemanticNumeric NumericUInt8) itemValue)
        Nothing -> assertEqual "dependency produces a hinted itemValue" True False
    Left _ -> assertEqual "dependency hint evaluation succeeds" True False

testDirectRuntimeWrapperUsesDisabledHost :: IO ()
testDirectRuntimeWrapperUsesDisabledHost = do
  let result =
        fmap
          (fmap renderRuntimeValue)
          (evaluateRuntimeExpr (hostCall "__kernel_readTextRaw!" [expressionLiteral (LText "disabled.jz")]))
  assertEqual
    "disabled host raw failure"
    (fmap (fmap renderRuntimeValue) (Right (Just (rawFailure HostUnsupported))))
    result

testHostBindingCacheSeparatesDynamicScopeInvocations :: IO ()
testHostBindingCacheSeparatesDynamicScopeInvocations = do
  let expression =
        expressionBlock
          [ statementLet
              "capture!"
              (SourceSpan 1 1)
              ( expressionLambda
                  "itemValue"
                  ( expressionBlock
                      [ statementLet
                          "local!"
                          (SourceSpan 2 1)
                          ( expressionBlock
                              [ statementExpression
                                  (SourceSpan 3 1)
                                  (hostCall "__kernel_writeStdoutRaw!" [expressionVariable "itemValue"]),
                                statementExpression (SourceSpan 4 1) (expressionVariable "itemValue")
                              ]
                          ),
                        statementExpression (SourceSpan 5 1) (expressionVariable "local!")
                      ]
                  )
              ),
            statementExpression
              (SourceSpan 6 1)
              ( expressionTuple
                  [ expressionApply (expressionVariable "capture!") (expressionLiteral (LText "first")),
                    expressionApply (expressionVariable "capture!") (expressionLiteral (LText "second"))
                  ]
              )
          ]
      (result, calls) = runState (evaluateRuntimeExprWithHost statefulHost expression) []
  assertEqual
    "dynamic host binding values"
    (Right (Just "(\"first\", \"second\")"))
    (fmap (fmap renderRuntimeValue) result)
  assertEqual
    "dynamic host binding calls"
    [WriteStdoutCall "first", WriteStdoutCall "second"]
    calls

testHostZeroArgumentImplMethod :: IO ()
testHostZeroArgumentImplMethod = do
  let expression =
        expressionBlock
          [ statementClass
              (SourceSpan 1 1)
              "RuntimeFlag"
              ["a"]
              [ classMethodSignature
                  "enabled!"
                  (SourceSpan 2 1)
                  (ConstrainedSignature [] TypeBool)
              ],
            statementImpl
              (SourceSpan 3 1)
              "RuntimeFlag"
              [TypeInt]
              [ implMethod
                  "enabled!"
                  (SourceSpan 4 1)
                  ( expressionBlock
                      [ statementExpression
                          (SourceSpan 5 1)
                          (hostCall "__kernel_writeStdoutRaw!" [expressionLiteral (LText "enabled")]),
                        statementExpression (SourceSpan 6 1) (expressionLiteral (LBool True))
                      ]
                  )
              ],
            statementExpression
              (SourceSpan 7 1)
              (expressionVariable (qualifiedName "RuntimeFlag" "enabled!"))
          ]
      (result, calls) = runState (evaluateRuntimeExprWithHost statefulHost expression) []
  assertRuntimeBool "zero-argument host method result" True result
  assertEqual "zero-argument host method call" [WriteStdoutCall "enabled"] calls

testNullaryEvidencePreservesHostMethodCaching :: IO ()
testNullaryEvidencePreservesHostMethodCaching = do
  callsRef <- newIORef []
  result <-
    runSourceWithPreludeAndHost
      (recordingIOHost callsRef)
      defaultWarningSettings
      Nothing
      """
      class RuntimeDefault(a) {
        defaultValue! :: a.
      }.
      impl RuntimeDefault(Int) {
        defaultValue! = { __kernel_writeStdoutRaw! "int". 41. }.
      }.
      impl RuntimeDefault(Bool) {
        defaultValue! = { __kernel_writeStdoutRaw! "bool". True. }.
      }.
      first! :: Int.
      first! = RuntimeDefault::defaultValue!.
      (first!, RuntimeDefault::defaultValue! @Bool, first!,
       RuntimeDefault::defaultValue! @Int, RuntimeDefault::defaultValue! @Int == 41).
      """
  calls <- readIORef callsRef
  assertEqual "nullary host compile errors" [] (runCompileErrors result)
  assertEqual "nullary host runtime errors" [] (runRuntimeErrors result)
  assertEqual "nullary host output" (Just "(41, True, 41, 41, True)") (runOutput result)
  assertEqual "each selected host method runs once" [WriteStdoutCall "int", WriteStdoutCall "bool"] calls

testDirectRuntimeWrapperRejectsDisabledExit :: IO ()
testDirectRuntimeWrapperRejectsDisabledExit = do
  let result = evaluateRuntimeExpr (hostCall "__kernel_exit!" [expressionLiteral (LInt 7)])
  assertLeftDiagnosticContains "disabled exit code" "E3031" result
  assertLeftDiagnosticContains "disabled exit message" "operation unsupported" result

testExitRejectsInvalidStatus :: IO ()
testExitRejectsInvalidStatus = do
  let (result, calls) =
        runState
          (evaluateRuntimeExprWithHost statefulHost (hostCall "__kernel_exit!" [expressionLiteral (LInt 256)]))
          []
  assertLeftDiagnosticContains "invalid exit status" "E3030" result
  assertLeftDiagnosticContains "invalid exit status range" "range 0..255" result
  assertEqual "invalid exit does not call host" [] calls

testStandaloneSourceInjectsRuntimeHost :: IO ()
testStandaloneSourceInjectsRuntimeHost = do
  callsRef <- newIORef []
  let host = recordingIOHost callsRef
  result <-
    runSourceWithPreludeAndHost
      host
      defaultWarningSettings
      Nothing
      "__kernel_writeStdoutRaw! \"standalone\"."
  calls <- readIORef callsRef
  assertEqual "standalone compile errors" [] (runCompileErrors result)
  assertEqual "standalone runtime errors" [] (runRuntimeErrors result)
  assertEqual "standalone raw output" (Just "(True, \"\", \"\", \"\")") (runOutput result)
  assertEqual "standalone host calls" [WriteStdoutCall "standalone"] calls

recordingIOHost :: IORef [HostCall] -> RuntimeHost IO
recordingIOHost callsRef =
  RuntimeHost
    { runtimeHostReadText = \path -> record (ReadTextCall path) (Right "file text"),
      runtimeHostWriteText = \path contents -> record (WriteTextCall path contents) (Right ()),
      runtimeHostReadStdin = record ReadStdinCall (Right "stdin text"),
      runtimeHostWriteStdout = \contents -> record (WriteStdoutCall contents) (Right ()),
      runtimeHostWriteStderr = \contents -> record (WriteStderrCall contents) (Right ()),
      runtimeHostArguments = record ArgumentsCall ["one", "two"],
      runtimeHostExit = \status -> record (ExitCall status) (Right RuntimeHostExitReturned)
    }
  where
    record call result = do
      modifyIORef' callsRef (<> [call])
      pure result

testProductionHostRoundTripsUtf8 :: IO ()
testProductionHostRoundTripsUtf8 =
  withTemporaryPath $ \path -> do
    writeResult <- runtimeHostWriteText productionRuntimeHost (Text.pack path) "Jazz λ 🎷"
    readResult <- runtimeHostReadText productionRuntimeHost (Text.pack path)
    assertEqual "production UTF-8 write" (Right ()) writeResult
    assertEqual "production UTF-8 read" (Right "Jazz λ 🎷") readResult

testProductionHostClassifiesMissingFile :: IO ()
testProductionHostClassifiesMissingFile =
  withTemporaryPath $ \path -> do
    removeFile path
    readResult <- runtimeHostReadText productionRuntimeHost (Text.pack path)
    assertEqual
      "production missing-file category"
      (Left (HostIOFailure HostNotFound (hostIOFailureMessage HostNotFound)))
      readResult

testProductionHostRejectsInvalidUtf8 :: IO ()
testProductionHostRejectsInvalidUtf8 =
  withTemporaryPath $ \path -> do
    ByteString.writeFile path (ByteString.pack [0xC3, 0x28])
    readResult <- runtimeHostReadText productionRuntimeHost (Text.pack path)
    assertEqual
      "production invalid UTF-8 category"
      (Left (HostIOFailure HostInvalidData (hostIOFailureMessage HostInvalidData)))
      readResult

testProductionHostExposesArguments :: IO ()
testProductionHostExposesArguments = do
  expected <- map Text.pack <$> getArgs
  actual <- runtimeHostArguments productionRuntimeHost
  assertEqual "production process arguments" expected actual

withTemporaryPath :: (FilePath -> IO a) -> IO a
withTemporaryPath action = do
  temporaryDirectory <- getTemporaryDirectory
  (path, handle) <- openBinaryTempFile temporaryDirectory "jazz-host-io"
  hClose handle
  action path `finally` removeIfPresent path
  where
    removeIfPresent path = do
      result <- runtimeHostReadText productionRuntimeHost (Text.pack path)
      case result of
        Left (HostIOFailure HostNotFound _) -> pure ()
        _ -> removeFile path

statefulHost :: RuntimeHost (State [HostCall])
statefulHost =
  RuntimeHost
    { runtimeHostReadText = \path -> record (ReadTextCall path) (Right "file text"),
      runtimeHostWriteText = \path contents -> record (WriteTextCall path contents) (Right ()),
      runtimeHostReadStdin = record ReadStdinCall (Right "stdin text"),
      runtimeHostWriteStdout = \contents -> record (WriteStdoutCall contents) (Right ()),
      runtimeHostWriteStderr = \contents -> record (WriteStderrCall contents) (Right ()),
      runtimeHostArguments = record ArgumentsCall ["one", "two"],
      runtimeHostExit = \status -> record (ExitCall status) (Right RuntimeHostExitReturned)
    }
  where
    record call result = do
      modify (<> [call])
      pure result

hostCall :: UnresolvedName -> [Expr 'Analyzed] -> Expr 'Analyzed
hostCall name = foldl expressionApply (expressionVariable name)

rawFailure :: HostIOCategory -> RuntimeValue
rawFailure category =
  VTuple
    [ VBool False,
      VText "",
      VText (hostIOCategoryToken category),
      VText (hostIOFailureMessage category)
    ]
