{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Runtime.Observation.ProfileTests
  ( tests,
  )
where

import Data.Aeson (Value, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Jazz.Compiler.AST
  ( CorePhase (Resolved),
    Expr,
    Literal (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol (BuiltinArguments, BuiltinHd, BuiltinTextLength),
    builtinSymbolKernelName,
  )
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.Driver
  ( ResolvedPrelude (PreludeAbsent),
    RunResult,
    runModuleGraphWithResolvedPreludeAndHostObserved,
    runRuntimeObservation,
  )
import Jazz.Compiler.ModuleResolver (ModuleResolutionConfig (..))
import Jazz.Compiler.Name
  ( Name (BuiltinName),
    mkIdentifier,
    operatorBindingName,
  )
import Jazz.Compiler.Runtime
  ( evaluateRuntimeExprObserved,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeCallableIdentity (..),
    RuntimeObservationReport (..),
    RuntimeObservationRequest (..),
    RuntimeObservationResult (..),
    RuntimeOutcome (..),
    RuntimeProfileEvent (..),
    RuntimeProfileFrame (..),
    RuntimeSemanticProfile (..),
    RuntimeStatistics (..),
    RuntimeTermination (..),
    runtimeCallableDisplayName,
  )
import Jazz.Compiler.Runtime.Observation.Profile
  ( encodeRuntimeSemanticProfile,
  )
import Jazz.Compiler.RuntimeHost (disabledRuntimeHost)
import Jazz.Compiler.Semantics.Runtime.Fixtures
  ( classMethodSignature,
    dataConstructor,
    expressionApply,
    expressionBinary,
    expressionBlock,
    expressionConstructor,
    expressionLambda,
    expressionList,
    expressionLiteral,
    expressionQualifiedMethod,
    expressionSectionRight,
    expressionTuple,
    expressionVariable,
    fixtureTypeVariable,
    implMethod,
    statementClass,
    statementData,
    statementExpression,
    statementImpl,
    statementLet,
  )
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.Compiler.WarningConfig (defaultWarningSettings)
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
  )
import System.Directory (doesFileExist)
import System.FilePath ((</>))

tests :: [NamedTest]
tests =
  [ ("semantic profiles use balanced Speedscope evented structure", testProfileStructure),
    ("semantic profile frames classify core callable kinds", testCallableIdentities),
    ("named closure frames use fully qualified binding identities", testNamedClosureIdentity),
    ("semantic profile encoding is byte deterministic", testProfileDeterminism),
    ("runtime failures close frames and mark profiles incomplete", testFailureProfile)
  ]

testProfileStructure :: IO ()
testProfileStructure = do
  report <-
    reportFor
      RuntimeObservationStatisticsAndProfile
      (expressionApply (expressionLambda "value" (expressionVariable "value")) (expressionLiteral (LInt 7)))
  profile <- requireProfile report
  assertEqual "profile termination" RuntimeSucceeded (runtimeSemanticProfileTermination profile)
  assertEqual "profile incomplete" False (runtimeSemanticProfileIncomplete profile)
  assertEqual
    "profile logical end"
    (runtimeEvaluatorTransitions (runtimeObservationStatistics report))
    (runtimeSemanticProfileEndValue profile)
  case runtimeSemanticProfileFrames profile of
    RuntimeProfileFrame RootCallable : _ -> pure ()
    frames -> failTest ("expected root frame first, got " <> Text.pack (show frames))
  assertBalancedEvents profile
  let encoded = encodeRuntimeSemanticProfile profile
  case eitherDecode encoded :: Either String Value of
    Left message -> failTest ("Speedscope JSON did not decode: " <> Text.pack message)
    Right _ -> pure ()
  assertBytesContain "schema marker" "https://www.speedscope.app/file-format-schema.json" encoded
  assertBytesContain "evented profile" "\"type\":\"evented\"" encoded
  assertBytesContain "logical unit" "\"unit\":\"none\"" encoded

testCallableIdentities :: IO ()
testCallableIdentities = do
  closureProfile <- profileFor (expressionApply (expressionLambda "value" (expressionVariable "value")) (expressionLiteral (LInt 1)))
  builtinProfile <-
    profileFor
      (expressionApply (kernelBuiltin BuiltinTextLength) (expressionLiteral (LText "Jazz")))
  operatorProfile <- profileFor (expressionBinary "+" (expressionLiteral (LInt 1)) (expressionLiteral (LInt 2)))
  constructorProfile <-
    profileFor
      ( expressionBlock
          [ statementData
              (SourceSpan 1 1)
              "Box"
              []
              [dataConstructor "Box" [TypeInt]],
            statementExpression (SourceSpan 2 1) (expressionApply (expressionConstructor "Box") (expressionLiteral (LInt 1)))
          ]
      )
  methodProfile <- profileFor qualifiedMethodExpression
  generatedProfile <- profileFor generatedSectionExpression
  hostProfile <- profileFor (expressionApply (kernelBuiltin BuiltinArguments) (expressionTuple []))
  assertHasIdentity "closure identity" isClosure closureProfile
  assertHasIdentity "builtin identity" (== BuiltinCallable "textLength") builtinProfile
  assertHasIdentity "operator identity" (== OperatorCallable "+") operatorProfile
  assertHasIdentity "constructor identity" isBoxConstructor constructorProfile
  assertHasIdentity "method identity" (== MethodCallable "Probe::identity") methodProfile
  assertHasIdentity
    "generated-section identity"
    (== GeneratedCallable "declared right section %%")
    generatedProfile
  assertHasIdentity "host identity" (== HostCallable "arguments") hostProfile
  where
    isClosure identity =
      case identity of
        ClosureCallable "<entry>" 1 "value" -> True
        _ -> False
    isBoxConstructor identity =
      case identity of
        ConstructorCallable name -> "Box" `Text.isSuffixOf` name
        _ -> False

testNamedClosureIdentity :: IO ()
testNamedClosureIdentity = do
  let fixtureRoot = "test/fixtures/runtime-observation/module-profile"
      resolutionConfig =
        ModuleResolutionConfig
          { moduleRoots = [fixtureRoot </> "src"],
            moduleExtension = ".jz"
          }
      lookupSource path = do
        exists <- doesFileExist path
        if exists then Just <$> TextIO.readFile path else pure Nothing
  result <-
    runModuleGraphWithResolvedPreludeAndHostObserved
      RuntimeObservationProfile
      disabledRuntimeHost
      defaultWarningSettings
      PreludeAbsent
      resolutionConfig
      ["App", "Main"]
      lookupSource
  report <- requireRunReport result
  profile <- requireProfile report
  assertHasIdentity
    "fully qualified first curried stage"
    (== ClosureCallable "App::Main::identity" 1 "left")
    profile
  assertHasIdentity
    "fully qualified second curried stage"
    (== ClosureCallable "App::Main::identity" 2 "right")
    profile

testProfileDeterminism :: IO ()
testProfileDeterminism = do
  let expression = expressionApply (expressionLambda "value" (expressionVariable "value")) (expressionLiteral (LInt 7))
  first <- profileFor expression
  second <- profileFor expression
  assertEqual "profile domain" first second
  assertEqual
    "profile bytes"
    (encodeRuntimeSemanticProfile first)
    (encodeRuntimeSemanticProfile second)

testFailureProfile :: IO ()
testFailureProfile = do
  let observed =
        evaluateRuntimeExprObserved
          RuntimeObservationStatisticsAndProfile
          (expressionApply (kernelBuiltin BuiltinHd) (expressionList []))
  case runtimeObservationOutcome observed of
    RuntimeOutcomeFailed _ -> pure ()
    outcome -> failTest ("expected runtime failure, got " <> Text.pack (show outcome))
  report <- requireObservedReport observed
  profile <- requireProfile report
  assertEqual "failed termination" RuntimeFailed (runtimeSemanticProfileTermination profile)
  assertEqual "failed profile is incomplete" True (runtimeSemanticProfileIncomplete profile)
  assertEqual
    "failed profile logical end"
    (runtimeEvaluatorTransitions (runtimeObservationStatistics report))
    (runtimeSemanticProfileEndValue profile)
  assertBalancedEvents profile
  assertBytesContain "incomplete profile name" "incomplete: failed" (encodeRuntimeSemanticProfile profile)

profileFor :: Expr 'Resolved -> IO RuntimeSemanticProfile
profileFor expression = do
  report <- reportFor RuntimeObservationProfile expression
  requireProfile report

reportFor :: RuntimeObservationRequest -> Expr 'Resolved -> IO RuntimeObservationReport
reportFor request expression = do
  let observed = evaluateRuntimeExprObserved request expression
  case runtimeObservationOutcome observed of
    RuntimeOutcomeCompleted _ -> pure ()
    outcome -> failTest ("expected runtime success, got " <> Text.pack (show outcome))
  requireObservedReport observed

requireObservedReport :: RuntimeObservationResult value -> IO RuntimeObservationReport
requireObservedReport observed =
  case runtimeObservationReport observed of
    Nothing -> failTest "expected a runtime observation report"
    Just report -> pure report

requireRunReport :: RunResult -> IO RuntimeObservationReport
requireRunReport result =
  case runRuntimeObservation result of
    Nothing -> failTest "expected a module runtime observation report"
    Just report -> pure report

requireProfile :: RuntimeObservationReport -> IO RuntimeSemanticProfile
requireProfile report =
  case runtimeObservationProfile report of
    Nothing -> failTest "expected a semantic runtime profile"
    Just profile -> pure profile

assertHasIdentity :: Text -> (RuntimeCallableIdentity -> Bool) -> RuntimeSemanticProfile -> IO ()
assertHasIdentity label matches profile =
  case find (matches . runtimeProfileFrameIdentity) (runtimeSemanticProfileFrames profile) of
    Just _ -> pure ()
    Nothing ->
      failTest
        ( label
            <> ": expected matching identity in "
            <> Text.pack (show (map (runtimeCallableDisplayName . runtimeProfileFrameIdentity) (runtimeSemanticProfileFrames profile)))
        )

assertBalancedEvents :: RuntimeSemanticProfile -> IO ()
assertBalancedEvents profile =
  go [] (runtimeSemanticProfileEvents profile)
  where
    go stack remainingEvents =
      case remainingEvents of
        [] -> assertEqual "closed semantic stack" [] stack
        RuntimeProfileOpen frameIndex _ : rest -> go (frameIndex : stack) rest
        RuntimeProfileClose frameIndex _ : rest ->
          case stack of
            openFrame : openRest
              | openFrame == frameIndex -> go openRest rest
            _ -> failTest ("unbalanced semantic profile event: " <> Text.pack (show (frameIndex, stack)))

assertBytesContain :: Text -> LazyByteString.ByteString -> LazyByteString.ByteString -> IO ()
assertBytesContain label expected actual =
  if Text.pack (LazyByteString.unpack expected) `Text.isInfixOf` Text.pack (LazyByteString.unpack actual)
    then pure ()
    else failTest (label <> ": expected " <> Text.pack (show expected) <> " in profile JSON")

kernelBuiltin :: BuiltinSymbol -> Expr 'Resolved
kernelBuiltin = expressionVariable . BuiltinName . mkIdentifier . builtinSymbolKernelName

qualifiedMethodExpression :: Expr 'Resolved
qualifiedMethodExpression =
  expressionBlock
    [ statementClass
        (SourceSpan 1 1)
        "Probe"
        ["a"]
        [ classMethodSignature
            "identity"
            (SourceSpan 2 1)
            ( ConstrainedSignature
                []
                (TypeFunction (fixtureTypeVariable "a") TypeBool)
            )
        ],
      statementImpl
        (SourceSpan 3 1)
        "Probe"
        [TypeInt]
        [ implMethod
            "identity"
            (SourceSpan 4 1)
            (expressionLambda "value" (expressionLiteral (LBool True)))
        ],
      statementImpl
        (SourceSpan 5 1)
        "Probe"
        [TypeNumeric NumericUInt8]
        [ implMethod
            "identity"
            (SourceSpan 6 1)
            (expressionLambda "value" (expressionLiteral (LBool False)))
        ],
      statementExpression
        (SourceSpan 7 1)
        (expressionApply (expressionQualifiedMethod "Probe" "identity") (expressionLiteral (LInt 1)))
    ]

generatedSectionExpression :: Expr 'Resolved
generatedSectionExpression =
  expressionBlock
    [ statementLet
        (operatorBindingName "%%")
        (SourceSpan 1 1)
        (expressionLambda "left" (expressionLambda "right" (expressionVariable "left"))),
      statementExpression
        (SourceSpan 2 1)
        (expressionApply (expressionSectionRight "%%" (expressionLiteral (LInt 2))) (expressionLiteral (LInt 1)))
    ]
