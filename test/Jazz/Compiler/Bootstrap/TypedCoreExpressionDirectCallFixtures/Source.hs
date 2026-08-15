{-# LANGUAGE OverloadedStrings #-}

-- | Fixture manifests, source programs, and module resolution.
module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Source where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (ResolveKernelOnly),
  )
import Jazz.Compiler.Diagnostics (Diagnostic)
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    resolveProgram,
  )
import Jazz.Compiler.TypeInference (InferenceInputs (..))
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType (TFunctionType, TIntType),
    TypeBinding (PlainTypeBinding),
    emptyScopeCapabilityFacts,
  )
import Jazz.Compiler.TypedCore
import Jazz.Compiler.WarningConfig (defaultWarningSettings)

data Fixture = Fixture
  { fixtureName :: Text,
    fixtureInputs :: InferenceInputs,
    fixtureSourcePath :: TypedSourcePath,
    fixtureSourceFiles :: Map.Map FilePath Text
  }

fixtureNames :: [Text]
fixtureNames = map fixtureName fixtures

acceptedFixtureNames :: [Text]
acceptedFixtureNames = map fixtureName acceptedFixtures

rejectedFixtureNames :: [Text]
rejectedFixtureNames = map fixtureName rejectedFixtures

priorScalarDirectCallFixtureNames :: [Text]
priorScalarDirectCallFixtureNames =
  [ "unit-entry",
    "bool-entry",
    "char-entry",
    "default-int-entry",
    "default-float-entry",
    "explicit-numeric-widths",
    "arithmetic-operators",
    "ordering-operators",
    "equality-operators",
    "scalar-parameter-return",
    "single-argument-direct-call",
    "curried-multi-argument-direct-call",
    "forward-direct-call-dag",
    "nested-direct-calls",
    "dollar-direct-call",
    "exported-direct-function",
    "source-diagnostic",
    "invalid-portable-source-path",
    "resolved-import",
    "ambient-prelude-input",
    "text-value",
    "list-value",
    "non-unit-tuple",
    "data-value",
    "conditional",
    "pattern-case",
    "local-block-binding",
    "named-function-value",
    "partial-direct-call",
    "oversaturated-direct-call",
    "capturing-function",
    "self-recursive-function",
    "mutually-recursive-functions",
    "polymorphic-or-evidence-function",
    "imported-direct-call",
    "user-defined-operator-call"
  ]

fixtures :: [Fixture]
fixtures = acceptedFixtures <> rejectedFixtures

acceptedFixtures :: [Fixture]
acceptedFixtures =
  [ sourceFixture "unit-entry" unitEntrySource,
    sourceFixture "bool-entry" boolEntrySource,
    sourceFixture "char-entry" charEntrySource,
    sourceFixture "default-int-entry" defaultIntEntrySource,
    sourceFixture "default-float-entry" defaultFloatEntrySource,
    sourceFixture "explicit-numeric-widths" explicitNumericWidthsSource,
    sourceFixture "arithmetic-operators" arithmeticOperatorsSource,
    sourceFixture "ordering-operators" orderingOperatorsSource,
    sourceFixture "equality-operators" equalityOperatorsSource,
    sourceFixtureNoExports "conditional" conditionalSource,
    sourceFixtureNoExports "pattern-case" patternCaseSource,
    sourceFixture "scalar-parameter-return" scalarParameterReturnSource,
    sourceFixture "single-argument-direct-call" singleArgumentDirectCallSource,
    sourceFixture "curried-multi-argument-direct-call" curriedMultiArgumentDirectCallSource,
    sourceFixture "three-argument-direct-call" threeArgumentDirectCallSource,
    sourceFixture "forward-direct-call-dag" forwardDirectCallDagSource,
    sourceFixture "nested-direct-calls" nestedDirectCallsSource,
    sourceFixture "dollar-direct-call" dollarDirectCallSource,
    sourceFixture "exported-direct-function" exportedDirectFunctionSource,
    sourceFixtureNoExports "named-function-value" namedFunctionValueSource,
    sourceFixtureNoExports "higher-order-call" higherOrderCallSource,
    sourceFixtureNoExports "closure-result" closureResultSource,
    sourceFixtureNoExports "callable-parameter-shadows-named-function" callableParameterShadowsNamedFunctionSource,
    sourceFixtureNoExports "callable-parameter-shadows-enclosing-function" callableParameterShadowsEnclosingFunctionSource,
    sourceFixtureNoExports "mixed-direct-and-value-use" mixedDirectAndValueUseSource,
    sourceFixtureNoExports "callable-parameter-value-shadows-enclosing-function" callableParameterValueShadowsEnclosingFunctionSource,
    sourceFixtureNoExports "capturing-function" capturingFunctionSource,
    sourceFixtureNoExports "partial-direct-call" partialDirectCallSource,
    sourceFixtureNoExports "self-recursive-function" selfRecursiveFunctionSource,
    sourceFixtureNoExports "mutually-recursive-functions" mutuallyRecursiveFunctionsSource,
    sourceFixtureNoExports "closure-value-mutual-recursion" closureValueMutualRecursionSource,
    sourceFixtureNoExports "closure-value-self-recursion" closureValueSelfRecursionSource,
    sourceFixtureNoExports "capturing-self-recursion" capturingSelfRecursionSource,
    sourceFixtureNoExports "capturing-mutual-recursion" capturingMutualRecursionSource
  ]

rejectedFixtures :: [Fixture]
rejectedFixtures =
  [ sourceFixture "source-diagnostic" sourceDiagnosticSource,
    (sourceFixture "invalid-portable-source-path" unitEntrySource)
      { fixtureSourcePath = TypedSourcePath "/private/host/Main.jz"
      },
    sourceFixtureWithFiles "resolved-import" emptyInputs resolvedImportSource resolvedImportSourceFiles,
    (sourceFixture "ambient-prelude-input" unitEntrySource)
      { fixtureInputs = ambientPreludeInputs
      },
    sourceFixtureNoExports "text-value" textValueSource,
    sourceFixtureNoExports "list-value" listValueSource,
    sourceFixtureNoExports "non-unit-tuple" nonUnitTupleSource,
    sourceFixtureNoExports "data-value" dataValueSource,
    sourceFixtureNoExports "local-block-binding" localBlockBindingSource,
    sourceFixtureNoExports "oversaturated-direct-call" oversaturatedDirectCallSource,
    sourceFixtureNoExports "later-capture-mutual-recursion" laterCaptureMutualRecursionSource,
    sourceFixtureNoExports "transitive-later-capture-mutual-recursion" transitiveLaterCaptureMutualRecursionSource,
    sourceFixtureNoExports "interleaved-rebound-capture-mutual-recursion" interleavedReboundCaptureMutualRecursionSource,
    sourceFixtureNoExports "polymorphic-or-evidence-function" polymorphicFunctionSource,
    (sourceFixture "imported-direct-call" importedDirectCallSource)
      { fixtureInputs =
          emptyInputs
            { inferenceImportedTypes =
                Map.singleton
                  "foreign"
                  (PlainTypeBinding (TFunctionType TIntType TIntType))
            }
      },
    sourceFixtureNoExports "user-defined-operator-call" userDefinedOperatorCallSource
  ]

forwardVisibilityNegativeFixtures :: [Fixture]
forwardVisibilityNegativeFixtures =
  [ sourceFixture "forward-polymorphic-function-invisibility" forwardPolymorphicFunctionSource,
    sourceFixture "forward-constrained-function-invisibility" forwardConstrainedFunctionSource,
    sourceFixture "forward-signed-scalar-invisibility" forwardSignedScalarSource,
    sourceFixture "forward-unsigned-lambda-invisibility" forwardUnsignedLambdaSource,
    sourceFixtureNoExports "nested-forward-signed-function-invisibility" nestedForwardSignedFunctionSource
  ]

ordinaryForwardVisibilityFixture :: Fixture
ordinaryForwardVisibilityFixture =
  sourceFixture "ordinary-unsigned-forward-caller-invisibility" ordinaryUnsignedForwardCallerSource

rejectedScalarFixtures :: [Fixture]
rejectedScalarFixtures = map fixtureByName ["text-value", "list-value", "non-unit-tuple", "data-value", "local-block-binding"]

explicitNumericTypes :: [Text]
explicitNumericTypes =
  [ "Int8",
    "Int16",
    "Int32",
    "Int64",
    "UInt8",
    "UInt16",
    "UInt32",
    "UInt64",
    "Float16",
    "Float32",
    "Float64"
  ]

sourceFixture :: Text -> Text -> Fixture
sourceFixture name source =
  Fixture
    name
    emptyInputs
    validSourcePath
    (Map.singleton sourceFilePath source)

sourceFixtureWithFiles :: Text -> InferenceInputs -> Text -> Map.Map FilePath Text -> Fixture
sourceFixtureWithFiles name inputs source additionalSources =
  Fixture
    name
    inputs
    validSourcePath
    (Map.insert sourceFilePath source additionalSources)

sourceFixtureNoExports :: Text -> Text -> Fixture
sourceFixtureNoExports name source =
  sourceFixture name (emptyExportModuleSource source)

resolveFixture :: Fixture -> IO (Either Diagnostic ModuleGraph.ResolvedModule)
resolveFixture fixture =
  resolveFixtureWithLookup fixture (pure . (`Map.lookup` fixtureSourceFiles fixture))

resolveFixtureWithLookup :: Fixture -> (FilePath -> IO (Maybe Text)) -> IO (Either Diagnostic ModuleGraph.ResolvedModule)
resolveFixtureWithLookup fixture loadSource =
  fmap (fmap resolverEntryModule) $
    resolveProgram
      fixtureResolverConfig
      (inferenceBuiltinMode (fixtureInputs fixture))
      Set.empty
      Set.empty
      loadSource
      modulePath
  where
    resolverEntryModule program =
      case filter ((== modulePath) . ModuleGraph.resolvedModulePath) (ModuleGraph.resolvedProgramModules program) of
        [resolvedModule] -> resolvedModule
        _ -> error "typed-core fixture resolver did not produce one entry module"

fixtureResolverConfig :: ModuleResolutionConfig
fixtureResolverConfig = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}

sourceFilePath :: FilePath
sourceFilePath = "src/App/Main.jz"

emptyExportModuleSource :: Text -> Text
emptyExportModuleSource source =
  "module App::Main () {\n" <> source <> "\n}"

fixtureByName :: Text -> Fixture
fixtureByName name =
  case filter ((== name) . fixtureName) fixtures of
    [fixture] -> fixture
    _ -> error ("typed-core fixture is missing or duplicated: " <> Text.unpack name)

explicitNumericWidthsSource :: Text
explicitNumericWidthsSource =
  Text.unlines
    [ "asInt8 :: Bool -> Int8.",
      "asInt8 = \\(ignored) -> 1.",
      "asInt16 :: Bool -> Int16.",
      "asInt16 = \\(ignored) -> 2.",
      "asInt32 :: Bool -> Int32.",
      "asInt32 = \\(ignored) -> 3.",
      "asInt64 :: Bool -> Int64.",
      "asInt64 = \\(ignored) -> 4.",
      "asUInt8 :: Bool -> UInt8.",
      "asUInt8 = \\(ignored) -> 5.",
      "asUInt16 :: Bool -> UInt16.",
      "asUInt16 = \\(ignored) -> 6.",
      "asUInt32 :: Bool -> UInt32.",
      "asUInt32 = \\(ignored) -> 7.",
      "asUInt64 :: Bool -> UInt64.",
      "asUInt64 = \\(ignored) -> 8.",
      "asFloat16 :: Bool -> Float16.",
      "asFloat16 = \\(ignored) -> 1.5.",
      "asFloat32 :: Bool -> Float32.",
      "asFloat32 = \\(ignored) -> 2.5.",
      "asFloat64 :: Bool -> Float64.",
      "asFloat64 = \\(ignored) -> 3.5.",
      "()."
    ]

scalarParameterReturnSource :: Text
scalarParameterReturnSource =
  Text.unlines
    [ "identity :: Int -> Int.",
      "identity = \\(item) -> item.",
      "identity 42."
    ]

singleArgumentDirectCallSource :: Text
singleArgumentDirectCallSource =
  Text.unlines
    [ "increment :: Int -> Int.",
      "increment = \\(item) -> item + 1.",
      "increment 41."
    ]

curriedMultiArgumentDirectCallSource :: Text
curriedMultiArgumentDirectCallSource =
  Text.unlines
    [ "combine :: Int -> Int -> Int.",
      "combine = \\(left, right) -> left + right.",
      "combine 20 22."
    ]

threeArgumentDirectCallSource :: Text
threeArgumentDirectCallSource =
  Text.unlines
    [ "sumThree :: Int -> Int -> Int -> Int.",
      "sumThree = \\(first, second, third) -> first + second + third.",
      "sumThree 10 20 12."
    ]

forwardDirectCallDagSource :: Text
forwardDirectCallDagSource =
  Text.unlines
    [ "first :: Int -> Int.",
      "first = \\(item) -> second item.",
      "second :: Int -> Int.",
      "second = \\(item) -> item + 1.",
      "first 41."
    ]

ordinaryUnsignedForwardCallerSource :: Text
ordinaryUnsignedForwardCallerSource =
  Text.unlines
    [ "caller = \\(item) -> later item.",
      "later :: Int -> Int.",
      "later = \\(item) -> item.",
      "caller 1."
    ]

forwardPolymorphicFunctionSource :: Text
forwardPolymorphicFunctionSource =
  Text.unlines
    [ "first :: Int -> Int.",
      "first = \\(item) -> later item.",
      "later :: a -> a.",
      "later = \\(item) -> item.",
      "first 1."
    ]

forwardConstrainedFunctionSource :: Text
forwardConstrainedFunctionSource =
  Text.unlines
    [ "class Eq(a) { }.",
      "impl Eq(Int) { }.",
      "first :: Int -> Int.",
      "first = \\(item) -> later item.",
      "later :: @{Eq(Int)}: Int -> Int.",
      "later = \\(item) -> item.",
      "first 1."
    ]

forwardSignedScalarSource :: Text
forwardSignedScalarSource =
  Text.unlines
    [ "first :: Int -> Int.",
      "first = \\(item) -> item + later.",
      "later :: Int.",
      "later = 1.",
      "first 1."
    ]

forwardUnsignedLambdaSource :: Text
forwardUnsignedLambdaSource =
  Text.unlines
    [ "first :: Int -> Int.",
      "first = \\(item) -> later item.",
      "later = \\(item) -> item.",
      "first 1."
    ]

nestedForwardSignedFunctionSource :: Text
nestedForwardSignedFunctionSource =
  Text.unlines
    [ "{",
      "  caller :: Int -> Int.",
      "  caller = \\(item) -> later True.",
      "  later :: Int -> Int.",
      "  later = \\(item) -> item.",
      "  caller 1.",
      "}."
    ]

nestedDirectCallsSource :: Text
nestedDirectCallsSource =
  Text.unlines
    [ "increment :: Int -> Int.",
      "increment = \\(item) -> item + 1.",
      "double :: Int -> Int.",
      "double = \\(item) -> item + item.",
      "double (increment 20)."
    ]

dollarDirectCallSource :: Text
dollarDirectCallSource =
  Text.unlines
    [ "increment :: Int -> Int.",
      "increment = \\(item) -> item + 1.",
      "increment $ 41."
    ]

exportedDirectFunctionSource :: Text
exportedDirectFunctionSource =
  "module App::Main (value increment) {\n"
    <> singleArgumentDirectCallSource
    <> "}\n"

namedFunctionValueSource :: Text
namedFunctionValueSource =
  Text.unlines
    [ "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "identity."
    ]

higherOrderCallSource :: Text
higherOrderCallSource =
  Text.unlines
    [ "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(function) -> function True.",
      "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "apply identity."
    ]

closureResultSource :: Text
closureResultSource =
  Text.unlines
    [ "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "choose :: Bool -> Bool -> Bool.",
      "choose = \\(ignored) -> identity.",
      "choose False."
    ]

callableParameterShadowsNamedFunctionSource :: Text
callableParameterShadowsNamedFunctionSource =
  Text.unlines
    [ "combine :: Bool -> Bool -> Bool.",
      "combine = \\(left, right) -> left.",
      "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(combine) -> combine True.",
      "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "apply identity."
    ]

callableParameterShadowsEnclosingFunctionSource :: Text
callableParameterShadowsEnclosingFunctionSource =
  Text.unlines
    [ "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(apply) -> apply True.",
      "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "apply identity."
    ]

mixedDirectAndValueUseSource :: Text
mixedDirectAndValueUseSource =
  Text.unlines
    [ "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(function) -> function True.",
      "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "apply identity == identity True."
    ]

callableParameterValueShadowsEnclosingFunctionSource :: Text
callableParameterValueShadowsEnclosingFunctionSource =
  Text.unlines
    [ "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(function) -> function True.",
      "forward :: (Bool -> Bool) -> Bool.",
      "forward = \\(forward) -> apply forward.",
      "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "forward identity."
    ]

partialDirectCallSource :: Text
partialDirectCallSource =
  Text.unlines
    [ "combine :: Int -> Int -> Int.",
      "combine = \\(left, right) -> left + right.",
      "combine 1."
    ]

oversaturatedDirectCallSource :: Text
oversaturatedDirectCallSource =
  Text.unlines
    [ "makeAdder :: Int -> Int -> Int.",
      "makeAdder = \\(left) -> (left +).",
      "makeAdder 1 2."
    ]

capturingFunctionSource :: Text
capturingFunctionSource =
  Text.unlines
    [ "seed :: Int.",
      "seed = 1.",
      "addSeed :: Int -> Int.",
      "addSeed = \\(item) -> item + seed.",
      "addSeed 41."
    ]

selfRecursiveFunctionSource :: Text
selfRecursiveFunctionSource =
  Text.unlines
    [ "loop :: Int -> Int.",
      "loop = \\(item) -> loop item.",
      "loop 1."
    ]

mutuallyRecursiveFunctionsSource :: Text
mutuallyRecursiveFunctionsSource =
  Text.unlines
    [ "left :: Int -> Int.",
      "left = \\(item) -> right item.",
      "right :: Int -> Int.",
      "right = \\(item) -> left item.",
      "left 1."
    ]

closureValueSelfRecursionSource :: Text
closureValueSelfRecursionSource =
  Text.unlines
    [ "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(function) -> function True.",
      "loop :: Bool -> Bool.",
      "loop = \\(item) -> apply loop.",
      "loop False."
    ]

closureValueMutualRecursionSource :: Text
closureValueMutualRecursionSource =
  Text.unlines
    [ "apply :: (Bool -> Bool) -> Bool.",
      "apply = \\(function) -> function True.",
      "left :: Bool -> Bool.",
      "left = \\(item) -> apply right.",
      "right :: Bool -> Bool.",
      "right = \\(item) -> apply left.",
      "left False."
    ]

capturingSelfRecursionSource :: Text
capturingSelfRecursionSource =
  Text.unlines
    [ "seed = 1.",
      "loop :: Int -> Int.",
      "loop = \\(item) -> loop (item + seed).",
      "loop 1."
    ]

capturingMutualRecursionSource :: Text
capturingMutualRecursionSource =
  Text.unlines
    [ "seed = 1.",
      "left :: Int -> Int.",
      "left = \\(item) -> right (item + seed).",
      "right :: Int -> Int.",
      "right = \\(item) -> left (item + seed).",
      "left 1."
    ]

laterCaptureMutualRecursionSource :: Text
laterCaptureMutualRecursionSource =
  Text.unlines
    [ "left :: Int -> Int.",
      "left = \\(item) -> right item.",
      "seed = 1.",
      "right :: Int -> Int.",
      "right = \\(item) -> left (item + seed).",
      "left 1."
    ]

transitiveLaterCaptureMutualRecursionSource :: Text
transitiveLaterCaptureMutualRecursionSource =
  Text.unlines
    [ "left :: Int -> Int.",
      "left = \\(item) -> right item.",
      "seed = 1.",
      "helper :: Int -> Int.",
      "helper = \\(item) -> item + seed.",
      "right :: Int -> Int.",
      "right = \\(item) -> left (helper item).",
      "left 1."
    ]

interleavedReboundCaptureMutualRecursionSource :: Text
interleavedReboundCaptureMutualRecursionSource =
  Text.unlines
    [ "seed = 1.",
      "left :: Int -> Int.",
      "left = \\(item) -> right (item + seed).",
      "seed = 2.",
      "right :: Int -> Int.",
      "right = \\(item) -> left (item + seed).",
      "left 1."
    ]

polymorphicFunctionSource :: Text
polymorphicFunctionSource =
  Text.unlines
    [ "identity :: a -> a.",
      "identity = \\(item) -> item.",
      "identity 1."
    ]

importedDirectCallSource :: Text
importedDirectCallSource =
  Text.unlines
    [ "foreign :: Int -> Int.",
      "foreign = \\(item) -> item.",
      "foreign 1."
    ]

userDefinedOperatorCallSource :: Text
userDefinedOperatorCallSource =
  Text.unlines
    [ "operator %% tier 2.",
      "(%%) :: Int -> Int -> Int.",
      "(%%) = \\(left, right) -> left + right.",
      "1 %% 2."
    ]

emptyInputs :: InferenceInputs
emptyInputs =
  InferenceInputs
    { inferenceBuiltinMode = ResolveKernelOnly,
      inferenceWarningSettings = defaultWarningSettings,
      inferenceImportedTypes = Map.empty,
      inferenceImportedDataTypes = Map.empty,
      inferenceImportedConstructorWitnessNames = Map.empty,
      inferenceImportedCapabilities = emptyScopeCapabilityFacts,
      inferenceImportedClassNames = Set.empty,
      inferenceCurrentModulePath = Just modulePath
    }

ambientPreludeInputs :: InferenceInputs
ambientPreludeInputs = emptyInputs {inferenceImportedClassNames = Set.singleton "PreludeClass"}

modulePath :: [Text]
modulePath = ["App", "Main"]

validSourcePath :: TypedSourcePath
validSourcePath = TypedSourcePath "src/App/Main.jz"

unitEntrySource, boolEntrySource, charEntrySource, defaultIntEntrySource, defaultFloatEntrySource, managedTextLiteralSource :: Text
unitEntrySource = "()."
boolEntrySource = "True."
charEntrySource = "'j'."
defaultIntEntrySource = "7."
defaultFloatEntrySource = "1.05."
managedTextLiteralSource = "\"managed\"."

managedTextEqualitySource, managedTextInequalitySource, managedTextLengthSource, managedTextAppendSource, managedTextAppendCharSource, managedTextCombinedOperationsSource, managedTextDuplicateEqualitySource, managedTextConditionalAppendSource, managedTextBareLengthSource, managedTextPartialAppendSource, managedTextPartialAppendCharSource, managedTextOversaturatedLengthSource, managedTextLiteralPatternSource, managedTextUnconsSource, managedTextFromCharsSource, managedTextConcatSource, managedTextReadIOSource, managedTextWriteIOSource :: Text
managedTextEqualitySource = "\"left\" == \"right\"."
managedTextInequalitySource = "\"left\" != \"right\"."
managedTextLengthSource = "__kernel_textLength \"Jazz\"."
managedTextAppendSource = "__kernel_textAppend \"Jazz\" \"!\"."
managedTextAppendCharSource = "__kernel_textAppendChar \"Jazz\" '!'."
managedTextCombinedOperationsSource =
  Text.unlines
    [ "\"left\" == \"right\".",
      "__kernel_textLength \"Jazz\".",
      "__kernel_textAppend \"Jazz\" \"!\".",
      "__kernel_textAppendChar \"Jazz\" '!'."
    ]
managedTextDuplicateEqualitySource =
  Text.unlines
    [ "\"left\" == \"right\".",
      "\"left\" == \"right\"."
    ]
managedTextConditionalAppendSource =
  "__kernel_textAppend (if True then \"left\" else \"other\") \"right\"."
managedTextBareLengthSource = "__kernel_textLength."
managedTextPartialAppendSource = "__kernel_textAppend \"Jazz\"."
managedTextPartialAppendCharSource = "__kernel_textAppendChar \"Jazz\"."
managedTextOversaturatedLengthSource = "__kernel_textLength \"Jazz\" 1."
managedTextLiteralPatternSource =
  "case \"Jazz\" { | \"Jazz\" -> True | _ -> False }."
managedTextUnconsSource = "__kernel_textUnconsRaw \"Jazz\"."
managedTextFromCharsSource = "__kernel_textFromChars ['J']."
managedTextConcatSource = "__kernel_textConcat [\"Jazz\"]."
managedTextReadIOSource = "__kernel_readTextRaw! \"source.jz\"."
managedTextWriteIOSource = "__kernel_writeTextRaw! \"target.jz\" \"Jazz\"."

scalarBindingLiteralSource, scalarBindingOrderedReuseSource, scalarBindingDirectCallResultSource, managedScalarBindingSource, scalarBindingFailedInitializerSource, managedTextIdentitySource, managedTextCaptureTransportSource, managedTextConditionalResultSource, managedTextScalarCaseResultSource :: Text
scalarBindingLiteralSource =
  Text.unlines
    [ "seed = 40.",
      "seed + 2."
    ]
scalarBindingOrderedReuseSource =
  Text.unlines
    [ "seed :: Int.",
      "seed = 40.",
      "answer = seed + 2.",
      "answer."
    ]
scalarBindingDirectCallResultSource =
  Text.unlines
    [ "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "answer = identity True.",
      "answer."
    ]
managedScalarBindingSource =
  Text.unlines
    [ "message = \"managed\".",
      "message."
    ]
managedTextIdentitySource =
  Text.unlines
    [ "identity :: Text -> Text.",
      "identity = \\(item) -> item.",
      "identity \"Jazz\"."
    ]
managedTextCaptureTransportSource =
  Text.unlines
    [ "message = \"managed\".",
      "capture :: Bool -> Text.",
      "capture = \\(ignored) -> message.",
      "capture True."
    ]
managedTextConditionalResultSource =
  Text.unlines
    [ "choose :: Bool -> Text.",
      "choose = \\(flag) -> if flag then \"yes\" else \"no\".",
      "choose True."
    ]
managedTextScalarCaseResultSource =
  Text.unlines
    [ "choose :: Bool -> Text.",
      "choose = \\(flag) -> case flag { | True -> \"yes\" | _ -> \"no\" }.",
      "choose True."
    ]
scalarBindingFailedInitializerSource =
  Text.unlines
    [ "failed = __kernel_toFloat64 1.",
      "failed."
    ]

arithmeticOperatorsSource, orderingOperatorsSource, equalityOperatorsSource :: Text
arithmeticOperatorsSource = Text.unlines ["1 + 2.", "3 - 1.", "2 * 4.", "8 / 2."]
orderingOperatorsSource = Text.unlines ["1 < 2.", "2 <= 2.", "3 > 2.", "3 >= 3."]
equalityOperatorsSource = Text.unlines ["1 == 1.", "1 != 2."]

sourceDiagnosticSource, textValueSource, listValueSource, nonUnitTupleSource, dataValueSource, conditionalSource, patternCaseSource, localBlockBindingSource :: Text
sourceDiagnosticSource = "missing."
textValueSource = Text.unlines ["\"managed\".", "[1]."]
listValueSource = "[1]."
nonUnitTupleSource = "(1, 2)."
dataValueSource = Text.unlines ["data Box = Box.", "Box."]
conditionalSource = "if True then 1 else 2."
patternCaseSource = "case True { | True -> 1 | _ -> 2 }."
localBlockBindingSource = "{ item = 1. item. }."

conditionalFunctionParameterSource, conditionalCapturedScalarSource, conditionalTailCallFunctionSource, conditionalClosureResultApplicationSource, nestedConditionalsSource :: Text
conditionalFunctionParameterSource =
  Text.unlines
    [ "choose :: Bool -> Int -> Int.",
      "choose = \\(flag, item) -> if flag then item else 0.",
      "choose True 7."
    ]
conditionalCapturedScalarSource =
  Text.unlines
    [ "seed :: Int.",
      "seed = 40.",
      "choose :: Bool -> Int.",
      "choose = \\(flag) -> if flag then seed else seed + 2.",
      "apply :: (Bool -> Int) -> Int.",
      "apply = \\(function) -> function True.",
      "apply choose."
    ]
conditionalTailCallFunctionSource =
  Text.unlines
    [ "loop :: Bool -> Int -> Int.",
      "loop = \\(stop, item) -> if stop then item else loop True item.",
      "loop False 7."
    ]
conditionalClosureResultApplicationSource =
  Text.unlines
    [ "identity :: Bool -> Bool.",
      "identity = \\(item) -> item.",
      "alwaysFalse :: Bool -> Bool.",
      "alwaysFalse = \\(item) -> False.",
      "(if True then identity else alwaysFalse) True."
    ]
nestedConditionalsSource =
  "if (if True then False else True) then (if True then 1 else 2) else (if False then 3 else 4)."

resolvedImportSource :: Text
resolvedImportSource = Text.unlines ["import Library::Value.", "()."]

resolvedImportSourceFiles :: Map.Map FilePath Text
resolvedImportSourceFiles = Map.singleton "src/Library/Value.jz" "answer = 1."

entryModule :: TypedModule
entryModule =
  TypedModule
    modulePath
    validSourcePath
    []
    []
    (TypedModuleInterface [] [] [] [])
    []
    [TypedExpressionStatement (TypedSpan 1 1) (TypedTupleExpr unitInfo [])]
    unitInfo

unitInfo :: TypedNodeInfo
unitInfo = TypedNodeInfo (TypedTupleType []) TypedUnitRecipe [] []

boolInfo, boolCallableInfo, charInfo, intInfo, floatInfo, textInfo :: TypedNodeInfo
boolInfo = TypedNodeInfo TypedBoolType TypedBoolRecipe [] []
boolCallableInfo =
  TypedNodeInfo
    (TypedFunctionType TypedBoolType TypedBoolType)
    (TypedClosureRecipe [TypedBoolRecipe] TypedBoolRecipe)
    []
    []
charInfo = TypedNodeInfo TypedCharType TypedCharRecipe [] []
intInfo = TypedNodeInfo TypedIntType (TypedSignedIntegerRecipe 64) [] []

inferredIntInfo :: TypedNodeInfo
inferredIntInfo = TypedNodeInfo (TypedNumericType TypedInt64Type) (TypedSignedIntegerRecipe 64) [] []

floatInfo = TypedNodeInfo TypedFloatType (TypedFloatRecipe 64) [] []

textInfo = TypedNodeInfo TypedTextType TypedManagedTextRecipe [] []

boolExpr :: Bool -> TypedExpr
boolExpr value = TypedLiteralExpr boolInfo (TypedBooleanLiteral value)

charExpr :: Char -> TypedExpr
charExpr value = TypedLiteralExpr charInfo (TypedCharacterLiteral value)

textExpr :: Text -> TypedExpr
textExpr value = TypedLiteralExpr textInfo (TypedTextLiteral value)

intExpr :: Integer -> TypedExpr
intExpr value = TypedLiteralExpr intInfo (TypedIntegerLiteral (Text.pack (show value)))

floatExpr :: Integer -> Text -> Maybe TypedNumericType -> TypedExpr
floatExpr whole fractional maybeNumericType = TypedLiteralExpr floatInfo (TypedFractionalLiteral (Text.pack (show whole)) fractional maybeNumericType)

binaryExpr :: TypedNodeInfo -> Text -> TypedExpr -> TypedExpr -> TypedExpr
binaryExpr resultInfo operator left right = TypedBinaryExpr resultInfo (TypedBuiltinOperator operator) left right

expectedScalarProgram :: TypedNodeInfo -> TypedExpr -> TypedProgram
expectedScalarProgram moduleInfo expression =
  TypedProgram Nothing [TypedModule modulePath validSourcePath [] [] (TypedModuleInterface [] [] [] []) [] [TypedExpressionStatement (TypedSpan 1 1) expression] moduleInfo] modulePath

expectedScalarStatements :: [TypedExpr] -> TypedProgram
expectedScalarStatements expressions =
  TypedProgram
    Nothing
    [ TypedModule
        modulePath
        validSourcePath
        []
        []
        (TypedModuleInterface [] [] [] [])
        []
        (zipWith (\line expression -> TypedExpressionStatement (TypedSpan line 1) expression) [1 ..] expressions)
        (typedExpressionInfo (last expressions))
    ]
    modulePath

data ExpectedFunction = ExpectedFunction
  { expectedFunctionName :: Text,
    expectedFunctionParameters :: [(Text, TypedNodeInfo)],
    expectedFunctionResult :: TypedNodeInfo,
    expectedFunctionShape :: TypedCallableShape,
    expectedFunctionBody :: TypedExpr
  }

identityFunction :: ExpectedFunction
identityFunction =
  ExpectedFunction
    "identity"
    [("item", intInfo)]
    intInfo
    TypedDirectCallableShape
    (variableExpr "item" intInfo)

boolIdentityFunction :: ExpectedFunction
boolIdentityFunction =
  ExpectedFunction
    "identity"
    [("item", boolInfo)]
    boolInfo
    TypedClosureCallableShape
    (variableExpr "item" boolInfo)

applyFunction :: ExpectedFunction
applyFunction =
  ExpectedFunction
    "apply"
    [("function", boolCallableInfo)]
    boolInfo
    TypedDirectCallableShape
    (directCall "function" [boolInfo] boolInfo [boolExpr True])

boolCombineFunction :: ExpectedFunction
boolCombineFunction =
  ExpectedFunction
    "combine"
    [("left", boolInfo), ("right", boolInfo)]
    boolInfo
    TypedDirectCallableShape
    (variableExpr "left" boolInfo)

applyCombineParameterFunction :: ExpectedFunction
applyCombineParameterFunction =
  ExpectedFunction
    "apply"
    [("combine", boolCallableInfo)]
    boolInfo
    TypedDirectCallableShape
    (directCall "combine" [boolInfo] boolInfo [boolExpr True])

selfShadowingApplyFunction :: ExpectedFunction
selfShadowingApplyFunction =
  ExpectedFunction
    "apply"
    [("apply", boolCallableInfo)]
    boolInfo
    TypedDirectCallableShape
    (directCall "apply" [boolInfo] boolInfo [boolExpr True])

shadowingForwardFunction :: ExpectedFunction
shadowingForwardFunction =
  ExpectedFunction
    "forward"
    [("forward", boolCallableInfo)]
    boolInfo
    TypedDirectCallableShape
    (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "forward" boolCallableInfo])

closurePassingLoopFunction :: ExpectedFunction
closurePassingLoopFunction =
  ExpectedFunction
    "loop"
    [("item", boolInfo)]
    boolInfo
    TypedClosureCallableShape
    (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "loop" boolCallableInfo])

nestedLambdaClosurePassingLoopFunction :: ExpectedFunction
nestedLambdaClosurePassingLoopFunction =
  ExpectedFunction
    "loop"
    [("item", boolInfo)]
    boolInfo
    TypedDirectCallableShape
    ( directCall
        "apply"
        [boolCallableInfo]
        boolInfo
        [ TypedLambdaExpr
            boolCallableInfo
            nestedParameterBinder
            nestedParameterName
            (directCall "loop" [boolInfo] boolInfo [variableExpr "nested" boolInfo])
        ]
    )
  where
    nestedParameterName = resolvedName "nested"
    nestedParameterBinder = TypedBinderId (modulePath, [3, 0, 0, 1], nestedParameterName)

closurePassingLeftFunction :: ExpectedFunction
closurePassingLeftFunction =
  ExpectedFunction
    "left"
    [("item", boolInfo)]
    boolInfo
    TypedClosureCallableShape
    (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "right" boolCallableInfo])

closurePassingRightFunction :: ExpectedFunction
closurePassingRightFunction =
  ExpectedFunction
    "right"
    [("item", boolInfo)]
    boolInfo
    TypedClosureCallableShape
    (directCall "apply" [boolCallableInfo] boolInfo [variableExpr "left" boolCallableInfo])

capturingLoopFunction :: ExpectedFunction
capturingLoopFunction = capturingRecursiveFunction "loop" "loop"

capturingLeftFunction :: ExpectedFunction
capturingLeftFunction = capturingRecursiveFunction "left" "right"

capturingRightFunction :: ExpectedFunction
capturingRightFunction = capturingRecursiveFunction "right" "left"

capturingRecursiveFunction :: Text -> Text -> ExpectedFunction
capturingRecursiveFunction functionName peerName =
  ExpectedFunction
    functionName
    [("item", intInfo)]
    intInfo
    TypedClosureCallableShape
    ( directCall
        peerName
        [intInfo]
        intInfo
        [binaryExpr intInfo "+" (variableExpr "item" intInfo) (variableExpr "seed" intInfo)]
    )

chooseFunction :: ExpectedFunction
chooseFunction =
  ExpectedFunction
    "choose"
    [("ignored", boolInfo)]
    boolCallableInfo
    TypedDirectCallableShape
    (variableExpr "identity" boolCallableInfo)

incrementFunction :: ExpectedFunction
incrementFunction = incrementNamed "increment"

incrementNamed :: Text -> ExpectedFunction
incrementNamed name =
  ExpectedFunction
    name
    [("item", intInfo)]
    intInfo
    TypedDirectCallableShape
    (binaryExpr intInfo "+" (variableExpr "item" intInfo) (intExpr 1))

combineFunction :: ExpectedFunction
combineFunction =
  ExpectedFunction
    "combine"
    [("left", intInfo), ("right", intInfo)]
    intInfo
    TypedDirectCallableShape
    (binaryExpr intInfo "+" (variableExpr "left" intInfo) (variableExpr "right" intInfo))

sumThreeFunction :: ExpectedFunction
sumThreeFunction =
  ExpectedFunction
    "sumThree"
    [("first", intInfo), ("second", intInfo), ("third", intInfo)]
    intInfo
    TypedDirectCallableShape
    ( binaryExpr
        intInfo
        "+"
        (binaryExpr intInfo "+" (variableExpr "first" intInfo) (variableExpr "second" intInfo))
        (variableExpr "third" intInfo)
    )

firstFunction :: ExpectedFunction
firstFunction =
  ExpectedFunction
    "first"
    [("item", intInfo)]
    intInfo
    TypedDirectCallableShape
    (directCall "second" [intInfo] intInfo [variableExpr "item" intInfo])

doubleFunction :: ExpectedFunction
doubleFunction =
  ExpectedFunction
    "double"
    [("item", intInfo)]
    intInfo
    TypedDirectCallableShape
    (binaryExpr intInfo "+" (variableExpr "item" intInfo) (variableExpr "item" intInfo))

explicitNumericFunctions :: [ExpectedFunction]
explicitNumericFunctions =
  [ numericFunction "asInt8" TypedInt8Type (TypedSignedIntegerRecipe 8) (TypedIntegerLiteral "1"),
    numericFunction "asInt16" TypedInt16Type (TypedSignedIntegerRecipe 16) (TypedIntegerLiteral "2"),
    numericFunction "asInt32" TypedInt32Type (TypedSignedIntegerRecipe 32) (TypedIntegerLiteral "3"),
    numericFunction "asInt64" TypedInt64Type (TypedSignedIntegerRecipe 64) (TypedIntegerLiteral "4"),
    numericFunction "asUInt8" TypedUInt8Type (TypedUnsignedIntegerRecipe 8) (TypedIntegerLiteral "5"),
    numericFunction "asUInt16" TypedUInt16Type (TypedUnsignedIntegerRecipe 16) (TypedIntegerLiteral "6"),
    numericFunction "asUInt32" TypedUInt32Type (TypedUnsignedIntegerRecipe 32) (TypedIntegerLiteral "7"),
    numericFunction "asUInt64" TypedUInt64Type (TypedUnsignedIntegerRecipe 64) (TypedIntegerLiteral "8"),
    numericFunction "asFloat16" TypedFloat16Type (TypedFloatRecipe 16) (TypedFractionalLiteral "1" "5" (Just TypedFloat16Type)),
    numericFunction "asFloat32" TypedFloat32Type (TypedFloatRecipe 32) (TypedFractionalLiteral "2" "5" (Just TypedFloat32Type)),
    numericFunction "asFloat64" TypedFloat64Type (TypedFloatRecipe 64) (TypedFractionalLiteral "3" "5" (Just TypedFloat64Type))
  ]
  where
    numericFunction name numericType recipe literal =
      let resultInfo = TypedNodeInfo (TypedNumericType numericType) recipe [] []
       in ExpectedFunction
            name
            [("ignored", boolInfo)]
            resultInfo
            TypedDirectCallableShape
            (TypedLiteralExpr resultInfo literal)

typedRecursiveGroupSchemaSentinel :: TypedRecursiveGroup
typedRecursiveGroupSchemaSentinel =
  TypedRecursiveGroup
    [TypedBinderId (modulePath, [1], resolvedName "loop")]

expectedFunctionProgram :: [Text] -> [ExpectedFunction] -> TypedExpr -> TypedProgram
expectedFunctionProgram = expectedFunctionProgramWithRecursiveGroups []

expectedFunctionProgramWithRecursiveGroups ::
  [[Text]] ->
  [Text] ->
  [ExpectedFunction] ->
  TypedExpr ->
  TypedProgram
expectedFunctionProgramWithRecursiveGroups = expectedFunctionProgramWithLineOffsetAndRecursiveGroups 0

expectedFunctionProgramWithLineOffset :: Int -> [Text] -> [ExpectedFunction] -> TypedExpr -> TypedProgram
expectedFunctionProgramWithLineOffset lineOffset = expectedFunctionProgramWithLineOffsetAndRecursiveGroups lineOffset []

expectedFunctionProgramWithLineOffsetAndRecursiveGroups :: Int -> [[Text]] -> [Text] -> [ExpectedFunction] -> TypedExpr -> TypedProgram
expectedFunctionProgramWithLineOffsetAndRecursiveGroups lineOffset recursiveGroupNames exportedNames functions terminalExpression =
  typedRecursiveGroupSchemaSentinel `seq`
    TypedProgram
      Nothing
      [ TypedModule
          modulePath
          validSourcePath
          []
          [TypedModuleExport TypedValueNamespace name | name <- exportedNames]
          typedInterface
          recursiveGroups
          statements
          (typedExpressionInfo boundTerminalExpression)
      ]
      modulePath
  where
    functionOwners =
      Map.fromList
        [ ( resolvedName (expectedFunctionName function),
            TypedBinderId (modulePath, [functionOffset * 2 + 1], resolvedName (expectedFunctionName function))
          )
        | (functionOffset, function) <- zip [0 ..] functions
        ]
    functionStatements =
      concat
        [ map
            (bindExpectedStatementVariables functionOwners)
            (expectedFunctionStatementsAtLineOffset lineOffset signatureIndex bindingIndex function)
        | (functionOffset, function) <- zip [0 ..] functions,
          let signatureIndex = functionOffset * 2,
          let bindingIndex = signatureIndex + 1
        ]
    terminalIndex = length functionStatements
    boundTerminalExpression = bindExpectedExpressionVariables functionOwners terminalExpression
    recursiveGroups =
      [ TypedRecursiveGroup
          [ functionOwners Map.! resolvedName name
          | name <- names
          ]
      | names <- recursiveGroupNames
      ]
    statements =
      functionStatements
        <> [TypedExpressionStatement (TypedSpan (lineOffset + terminalIndex + 1) 1) boundTerminalExpression]
    typedInterface =
      TypedModuleInterface
        [ TypedValueInterface
            (resolvedName name)
            (functionScheme bindingIndex function)
        | name <- exportedNames,
          (functionOffset, function) <- zip [0 ..] functions,
          expectedFunctionName function == name,
          let bindingIndex = functionOffset * 2 + 1
        ]
        []
        []
        []

expectedFunctionStatements :: Int -> Int -> ExpectedFunction -> [TypedStatement]
expectedFunctionStatements = expectedFunctionStatementsAtLineOffset 0

expectedFunctionStatementsAtLineOffset :: Int -> Int -> Int -> ExpectedFunction -> [TypedStatement]
expectedFunctionStatementsAtLineOffset lineOffset signatureIndex bindingIndex function =
  [ TypedSignatureStatement
      signatureOwner
      functionName
      (TypedSpan (lineOffset + signatureIndex + 1) 1)
      (functionScheme signatureIndex function),
    TypedLetStatement
      bindingOwner
      functionName
      (TypedSpan (lineOffset + bindingIndex + 1) 1)
      (functionScheme bindingIndex function)
      (lambdaExpression bindingIndex [0] (expectedFunctionParameters function))
  ]
  where
    functionName = resolvedName (expectedFunctionName function)
    signatureOwner = TypedBinderId (modulePath, [signatureIndex], functionName)
    bindingOwner = TypedBinderId (modulePath, [bindingIndex], functionName)

    lambdaExpression statementIndex childPath parameters =
      case parameters of
        [] -> expectedFunctionBody function
        (parameterName, _) : rest ->
          let typedParameterName = resolvedName parameterName
              parameterBinder = TypedBinderId (modulePath, statementIndex : childPath, typedParameterName)
           in TypedLambdaExpr
                (expectedFunctionInfo function parameters)
                parameterBinder
                typedParameterName
                (lambdaExpression statementIndex (childPath <> [0]) rest)

functionScheme :: Int -> ExpectedFunction -> TypedScheme
functionScheme statementIndex function =
  let functionName = resolvedName (expectedFunctionName function)
      owner = TypedBinderId (modulePath, [statementIndex], functionName)
      info = expectedFunctionInfo function (expectedFunctionParameters function)
   in TypedScheme owner [] [] [] (typedExpressionType info) (typedExpressionRecipe info) (Just (expectedFunctionShape function))

expectedFunctionInfo :: ExpectedFunction -> [(Text, TypedNodeInfo)] -> TypedNodeInfo
expectedFunctionInfo function parameters =
  case expectedFunctionShape function of
    TypedDirectCallableShape -> functionInfo parameters (expectedFunctionResult function)
    TypedClosureCallableShape -> stagedFunctionInfo parameters (expectedFunctionResult function)

functionInfo :: [(Text, TypedNodeInfo)] -> TypedNodeInfo -> TypedNodeInfo
functionInfo parameters resultInfo =
  TypedNodeInfo
    (foldr (TypedFunctionType . typedExpressionType . snd) (typedExpressionType resultInfo) parameters)
    ( case parameters of
        [] -> typedExpressionRecipe resultInfo
        _ ->
          TypedClosureRecipe
            (map (typedExpressionRecipe . snd) parameters)
            (typedExpressionRecipe resultInfo)
    )
    []
    []

stagedFunctionInfo :: [(Text, TypedNodeInfo)] -> TypedNodeInfo -> TypedNodeInfo
stagedFunctionInfo parameters resultInfo =
  TypedNodeInfo
    (foldr (TypedFunctionType . typedExpressionType . snd) (typedExpressionType resultInfo) parameters)
    ( foldr
        (\(_, parameterInfo) resultRecipe -> TypedClosureRecipe [typedExpressionRecipe parameterInfo] resultRecipe)
        (typedExpressionRecipe resultInfo)
        parameters
    )
    []
    []

directCall :: Text -> [TypedNodeInfo] -> TypedNodeInfo -> [TypedExpr] -> TypedExpr
directCall functionName parameterInfos resultInfo arguments =
  go
    (TypedVariableExpr (functionInfo (zip (repeat "") parameterInfos) resultInfo) (resolvedName functionName) Nothing)
    parameterInfos
    arguments
  where
    go functionExpression remainingParameters remainingArguments =
      case (remainingParameters, remainingArguments) of
        (_ : parameterRest, argument : argumentRest) ->
          let applicationInfo =
                case parameterRest of
                  [] -> resultInfo
                  _ -> stagedFunctionInfo (zip (repeat "") parameterRest) resultInfo
           in go (TypedApplyExpr applicationInfo functionExpression argument) parameterRest argumentRest
        ([], []) -> functionExpression
        _ -> error "expected direct call must be fully saturated"

resolvedName :: Text -> TypedCoreName
resolvedName = TypedResolvedName TypedCurrentModule TypedValueNamespace

variableExpr :: Text -> TypedNodeInfo -> TypedExpr
variableExpr name info = TypedVariableExpr info (resolvedName name) Nothing

bindExpectedStatementVariables :: Map.Map TypedCoreName TypedBinderId -> TypedStatement -> TypedStatement
bindExpectedStatementVariables bindings statement =
  case statement of
    TypedLetStatement owner name spanValue schemeValue expression ->
      TypedLetStatement owner name spanValue schemeValue (bindExpectedExpressionVariables bindings expression)
    TypedExpressionStatement spanValue expression ->
      TypedExpressionStatement spanValue (bindExpectedExpressionVariables bindings expression)
    other -> other

bindExpectedExpressionVariables :: Map.Map TypedCoreName TypedBinderId -> TypedExpr -> TypedExpr
bindExpectedExpressionVariables bindings expression =
  case expression of
    TypedLiteralExpr {} -> expression
    TypedVariableExpr info name _ -> TypedVariableExpr info name (Map.lookup name bindings)
    TypedLambdaExpr info owner name body ->
      TypedLambdaExpr info owner name (bindExpectedExpressionVariables (Map.insert name owner bindings) body)
    TypedOperatorValueExpr {} -> expression
    TypedListExpr info values -> TypedListExpr info (map recurse values)
    TypedTupleExpr info values -> TypedTupleExpr info (map recurse values)
    TypedApplyExpr info function argument -> TypedApplyExpr info (recurse function) (recurse argument)
    TypedTypeApplicationExpr info function spanValue typeValue -> TypedTypeApplicationExpr info (recurse function) spanValue typeValue
    TypedIfExpr info condition consequent alternative -> TypedIfExpr info (recurse condition) (recurse consequent) (recurse alternative)
    TypedPatternCaseExpr info scrutinee arms ->
      TypedPatternCaseExpr info (recurse scrutinee) (map bindArm arms)
    TypedBinaryExpr info operator left right -> TypedBinaryExpr info operator (recurse left) (recurse right)
    TypedLeftSectionExpr info left operator -> TypedLeftSectionExpr info (recurse left) operator
    TypedRightSectionExpr info operator right -> TypedRightSectionExpr info operator (recurse right)
    TypedBlockExpr info statements -> TypedBlockExpr info (map (bindExpectedStatementVariables bindings) statements)
  where
    recurse = bindExpectedExpressionVariables bindings
    bindArm (TypedCaseArm patternValue guard result) =
      TypedCaseArm patternValue (recurse <$> guard) (recurse result)

typedExpressionType :: TypedNodeInfo -> TypedType
typedExpressionType (TypedNodeInfo expressionType _ _ _) = expressionType

typedExpressionRecipe :: TypedNodeInfo -> TypedRepresentationRecipe
typedExpressionRecipe (TypedNodeInfo _ recipe _ _) = recipe
