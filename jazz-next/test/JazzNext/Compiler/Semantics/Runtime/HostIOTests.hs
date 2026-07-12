{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.Runtime.HostIOTests
  ( hostIOTests
  ) where

import Control.Monad.Trans.State.Strict
  ( State,
    modify,
    runState
  )
import Control.Exception (finally)
import qualified Data.ByteString as ByteString
import Data.Functor.Identity (Identity (..))
import Data.Either (isRight)
import Data.IORef
  ( IORef,
    newIORef,
    readIORef,
    modifyIORef'
  )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import JazzNext.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    Literal (..),
    NumericType (..),
    Pattern (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog (BuiltinResolutionMode (..))
import JazzNext.Compiler.Diagnostics (SourceSpan (..))
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runSourceWithPreludeAndHost
  )
import JazzNext.Compiler.Name (Name)
import JazzNext.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeValue (..),
    evaluateModuleScopeWithRequiredHost,
    evaluateRuntimeExpr,
    evaluateRuntimeExprWithHost,
    renderRuntimeValue,
    runtimeValueExactlyMatchesConstraint
  )
import JazzNext.Compiler.RuntimeHost
  ( HostIOCategory (..),
    HostIOFailure (..),
    RuntimeHost (..),
    hostIOCategoryToken,
    hostIOFailureMessage,
    productionRuntimeHost
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftDiagnosticContains
  )
import JazzNext.Compiler.WarningConfig (defaultWarningSettings)
import System.Directory
  ( getTemporaryDirectory,
    removeFile
  )
import System.Environment (getArgs)
import System.IO
  ( hClose,
    openBinaryTempFile
  )

hostIOTests :: [NamedTest]
hostIOTests =
  [ ("host-aware evaluator preserves pure expressions", testHostAwareEvaluatorPreservesPureExpressions),
    ("host intrinsics return raw values and preserve call order", testHostIntrinsicsReturnRawValues),
    ("host failures normalize every category", testHostFailuresNormalizeEveryCategory),
    ("host effects execute at selected expression depth", testHostEffectsExecuteAtSelectedExpressionDepth),
    ("host-dependent function selectors use the injected host", testHostDependentFunctionSelector),
    ("host scopes preserve mutually recursive functions", testHostScopePreservesMutualRecursion),
    ("host scopes preserve binding signature hints", testHostScopePreservesBindingSignatureHints),
    ("host dependency scopes keep unused bindings lazy", testHostDependencyScopeKeepsUnusedBindingLazy),
    ("direct runtime wrappers normalize disabled host calls", testDirectRuntimeWrapperUsesDisabledHost),
    ("exit rejects statuses outside the portable range", testExitRejectsInvalidStatus),
    ("standalone source execution injects its runtime host", testStandaloneSourceInjectsRuntimeHost),
    ("production host round trips multibyte UTF-8", testProductionHostRoundTripsUtf8),
    ("production host classifies missing files", testProductionHostClassifiesMissingFile),
    ("production host rejects invalid UTF-8", testProductionHostRejectsInvalidUtf8),
    ("production host exposes process arguments", testProductionHostExposesArguments)
  ]

testHostAwareEvaluatorPreservesPureExpressions :: IO ()
testHostAwareEvaluatorPreservesPureExpressions = do
  mapM_ assertPreserved expressions
  where
    expressions =
      [ EBinary "+" (ELit (LInt 20)) (ELit (LInt 22)),
        EApply (ELambda "value" (EBinary "+" (EVar "value") (ELit (LInt 2)))) (ELit (LInt 40)),
        EBlock
          [ SLet "value" (SourceSpan 1 1) (ELit (LInt 40)),
            SExpr (SourceSpan 2 1) (EBinary "+" (EVar "value") (ELit (LInt 2)))
          ]
      ]

    assertPreserved expression = do
      let expected = evaluateRuntimeExpr expression
          actual = runIdentity (evaluateRuntimeExprWithHost deterministicHost expression)
      assertEqual "host-aware pure result" expected actual

deterministicHost :: RuntimeHost Identity
deterministicHost =
  RuntimeHost
    { runtimeHostReadText = \_ -> pure (Right "unused"),
      runtimeHostWriteText = \_ _ -> pure (Right ()),
      runtimeHostReadStdin = pure (Right "unused"),
      runtimeHostWriteStdout = \_ -> pure (Right ()),
      runtimeHostWriteStderr = \_ -> pure (Right ()),
      runtimeHostArguments = pure [],
      runtimeHostExit = \_ -> pure ()
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
        [ hostCall "__kernel_readTextRaw!" [ELit (LText "source.jz")],
          hostCall "__kernel_writeTextRaw!" [ELit (LText "output.txt"), ELit (LText "Jazz")],
          hostCall "__kernel_readStdinRaw!" [ETuple []],
          hostCall "__kernel_writeStdoutRaw!" [ELit (LText "out")],
          hostCall "__kernel_writeStderrRaw!" [ELit (LText "err")],
          hostCall "__kernel_arguments!" [ETuple []],
          hostCall "__kernel_exit!" [ELit (LInt 7)]
        ]
      (results, calls) = runState (traverse (evaluateRuntimeExprWithHost statefulHost) expressions) []
      success payload = Right (Just (rawSuccess payload))
  assertEqual
    "host intrinsic raw values"
    [ success "file text",
      success "",
      success "stdin text",
      success "",
      success "",
      Right (Just (VList [VText "one", VText "two"] (Just (TypeList TypeText)))),
      Right (Just (VTuple []))
    ]
    results
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
          expression = hostCall "__kernel_readTextRaw!" [ELit (LText "missing.jz")]
          actual = runIdentity (evaluateRuntimeExprWithHost host expression)
          expected = Right (Just (rawFailure category))
      assertEqual "normalized host failure category" expected actual

testHostEffectsExecuteAtSelectedExpressionDepth :: IO ()
testHostEffectsExecuteAtSelectedExpressionDepth = do
  let expressions =
        [ EApply
            (ELambda "value" (hostCall "__kernel_writeStdoutRaw!" [EVar "value"]))
            (ELit (LText "closure")),
          EIf
            (ELit (LBool False))
            (hostCall "__kernel_writeStderrRaw!" [ELit (LText "skipped")])
            (hostCall "__kernel_writeStdoutRaw!" [ELit (LText "branch")]),
          EPatternCase
            (ELit (LBool True))
            [ CaseArm (PLiteral (LBool True)) Nothing (hostCall "__kernel_writeStderrRaw!" [ELit (LText "arm")]),
              CaseArm PWildcard Nothing (hostCall "__kernel_writeStderrRaw!" [ELit (LText "fallback")])
            ],
          EBlock
            [ SExpr (SourceSpan 1 1) (hostCall "__kernel_writeStdoutRaw!" [ELit (LText "block")])
            ]
        ]
      (results, calls) = runState (traverse (evaluateRuntimeExprWithHost statefulHost) expressions) []
  assertEqual "nested effect results" (replicate 4 (Right (Just (rawSuccess "")))) results
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
        EBinary
          "=="
          (hostCall "__kernel_arguments!" [ETuple []])
          (EList [ELit (LText "one"), ELit (LText "two")])
      expression =
        EBlock
          [ SLet
              "choose!"
              (SourceSpan 1 1)
              ( EIf
                  selector
                  (ELambda "ignored" (ELit (LInt 1)))
                  (ELambda "ignored" (ELit (LInt 2)))
              ),
            SExpr (SourceSpan 2 1) (EApply (EVar "choose!") (ETuple []))
          ]
      (result, calls) = runState (evaluateRuntimeExprWithHost statefulHost expression) []
  assertEqual "host-selected closure result" (Right (Just "1")) (fmap (fmap renderRuntimeValue) result)
  assertEqual "host selector call" [ArgumentsCall] calls

testHostScopePreservesMutualRecursion :: IO ()
testHostScopePreservesMutualRecursion = do
  let decrement name = EApply (EVar name) (EBinary "-" (EVar "value") (ELit (LInt 1)))
      isZero = EBinary "==" (EVar "value") (ELit (LInt 0))
      expression =
        EBlock
          [ SLet
              "even"
              (SourceSpan 1 1)
              (ELambda "value" (EIf isZero (ELit (LBool True)) (decrement "odd"))),
            SLet
              "odd"
              (SourceSpan 2 1)
              (ELambda "value" (EIf isZero (ELit (LBool False)) (decrement "even"))),
            SExpr (SourceSpan 3 1) (hostCall "__kernel_writeStdoutRaw!" [ELit (LText "once")]),
            SExpr (SourceSpan 4 1) (EApply (EVar "even") (ELit (LInt 4)))
          ]
      (result, calls) = runState (evaluateRuntimeExprWithHost statefulHost expression) []
  assertEqual "mutually recursive result" (Right (Just (VBool True))) result
  assertEqual "unrelated host call" [WriteStdoutCall "once"] calls

testHostScopePreservesBindingSignatureHints :: IO ()
testHostScopePreservesBindingSignatureHints = do
  let expression =
        EBlock
          [ SSignature "value" (SourceSpan 1 1) (SignatureType (TypeNumeric NumericInt8)),
            SLet "value" (SourceSpan 2 1) (ELit (LInt 1)),
            SExpr (SourceSpan 3 1) (hostCall "__kernel_writeStdoutRaw!" [ELit (LText "once")]),
            SExpr (SourceSpan 4 1) (EVar "value")
          ]
      (result, calls) = runState (evaluateRuntimeExprWithHost statefulHost expression) []
  assertEqual "signature host call" [WriteStdoutCall "once"] calls
  case result of
    Right (Just value) ->
      assertEqual
        "host scope keeps Int8 runtime hint"
        True
        (runtimeValueExactlyMatchesConstraint (TypeNumeric NumericInt8) value)
    _ -> assertEqual "host scope produces signed value" True False

testHostDependencyScopeKeepsUnusedBindingLazy :: IO ()
testHostDependencyScopeKeepsUnusedBindingLazy = do
  let statements =
        [ SLet
            "unused!"
            (SourceSpan 1 1)
            (hostCall "__kernel_writeStdoutRaw!" [ELit (LText "unused")])
        ]
      (result, calls) =
        runState
          ( evaluateModuleScopeWithRequiredHost
              statefulHost
              Nothing
              EvaluateDependencyModule
              ResolveKernelOnly
              Map.empty
              Map.empty
              statements
          )
          []
  assertEqual "dependency scope result" True (isRight result)
  assertEqual "unused dependency host calls" [] calls

testDirectRuntimeWrapperUsesDisabledHost :: IO ()
testDirectRuntimeWrapperUsesDisabledHost =
  assertEqual
    "disabled host raw failure"
    (Right (Just (rawFailure HostUnsupported)))
    (evaluateRuntimeExpr (hostCall "__kernel_readTextRaw!" [ELit (LText "disabled.jz")]))

testExitRejectsInvalidStatus :: IO ()
testExitRejectsInvalidStatus = do
  let (result, calls) =
        runState
          (evaluateRuntimeExprWithHost statefulHost (hostCall "__kernel_exit!" [ELit (LInt 256)]))
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
      runtimeHostExit = \status -> record (ExitCall status) ()
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
  (path, handle) <- openBinaryTempFile temporaryDirectory "jazz-next-host-io"
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
      runtimeHostExit = \status -> record (ExitCall status) ()
    }
  where
    record call result = do
      modify (<> [call])
      pure result

hostCall :: Name -> [Expr] -> Expr
hostCall name = foldl EApply (EVar name)

rawSuccess :: Text -> RuntimeValue
rawSuccess payload = VTuple [VBool True, VText payload, VText "", VText ""]

rawFailure :: HostIOCategory -> RuntimeValue
rawFailure category =
  VTuple
    [ VBool False,
      VText "",
      VText (hostIOCategoryToken category),
      VText (hostIOFailureMessage category)
    ]
