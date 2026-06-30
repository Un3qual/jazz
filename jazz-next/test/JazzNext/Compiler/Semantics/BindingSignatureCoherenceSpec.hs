{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( ClassMethodSignature (..),
    ConstraintSignatureType (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr,
    compileSource,
    compileSourceWithPrelude
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    assertSingleDiagnosticPrimarySpan,
    assertSingleDiagnosticRelatedSpan,
    assertSingleDiagnosticSubject,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "BindingSignatureCoherence" tests

tests :: [NamedTest]
tests =
  [ ("signature directly above matching binding is accepted", testSignatureDirectlyAboveBinding),
    ("signature type mismatch is rejected", testSignatureTypeMismatch),
    ("signature separated from binding by expression is rejected", testSignatureSeparatedFromBinding),
    ("signature must match immediate binding name", testSignatureNameMismatch),
    ("use-before-definition is rejected", testUseBeforeDefinition),
    ("nested scope resolves outer bindings", testNestedScopeResolvesOuterBinding),
    ("self-recursive binding is accepted", testSelfRecursiveBinding),
    ("mutual recursion group is accepted", testMutualRecursionGroup),
    ("three-node mutual recursion group is accepted", testThreeNodeMutualRecursionGroup),
    ("non-recursive forward reference in bindings is rejected", testNonRecursiveForwardReference),
    ("rebinding cannot retroactively create recursion group", testRebindingDoesNotCreateRetroactiveRecursion),
    ("source pipeline accepts adjacent signature and binding", testSourceAcceptsSignatureAdjacency),
    ("source pipeline accepts inert class and impl declarations", testSourceAcceptsCapabilityDeclarations),
    ("source pipeline accepts class method signature metadata", testSourceAcceptsClassMethodSignatureMetadata),
    ("source pipeline rejects duplicate class method signatures", testSourceRejectsDuplicateClassMethodSignatures),
    ("analyzer rejects duplicate class method metadata", testAnalyzerRejectsDuplicateClassMethodMetadata),
    ("source pipeline analyzes impl method binding metadata", testSourceAnalyzesImplMethodBindingMetadata),
    ("source pipeline rejects variable-target impl method bindings", testSourceRejectsVariableTargetImplMethodBindings),
    ("source pipeline rejects variable-target empty impl declarations", testSourceRejectsVariableTargetEmptyImplDeclarations),
    ("source pipeline instantiates ordinary binding schemes per use", testSourceInstantiatesOrdinaryBindingSchemesPerUse),
    ("source pipeline instantiates unconstrained variables beside numeric constraints per use", testSourceInstantiatesUnconstrainedNumericBindingVariablesPerUse),
    ("source pipeline instantiates unconstrained variables beside equality constraints per use", testSourceInstantiatesUnconstrainedEqualityBindingVariablesPerUse),
    ("source pipeline instantiates recursive binding schemes per use", testSourceInstantiatesRecursiveBindingSchemesPerUse),
    ("source pipeline instantiates mutual recursive binding schemes per use", testSourceInstantiatesMutualRecursiveBindingSchemesPerUse),
    ("source pipeline instantiates interleaved mutual recursive schemes per use", testSourceInstantiatesInterleavedMutualRecursiveBindingSchemesPerUse),
    ("source pipeline keeps later rebinding over recursive scheme", testSourceKeepsLaterRebindingOverRecursiveScheme),
    ("source pipeline rejects interleaved use constrained by later recursive member", testSourceRejectsInterleavedUseConstrainedByLaterRecursiveMember),
    ("source pipeline types recursive guards against prior rebinding", testSourceTypesRecursiveGuardsAgainstPriorRebinding),
    ("source pipeline defers partial recursive previews past intervening dependencies", testSourceDefersPartialRecursivePreviewsPastInterveningDependencies),
    ("source pipeline previews through intervening recursive group members", testSourcePreviewsThroughInterveningRecursiveGroupMembers),
    ("source pipeline rejects duplicate impl method bindings", testSourceRejectsDuplicateImplMethodBindings),
    ("source pipeline rejects non-binding impl body items", testSourceRejectsNonBindingImplBodyItem),
    ("source pipeline accepts single-target qualified method dispatch", testSourceAcceptsSingleTargetQualifiedMethodDispatch),
    ("source pipeline selects qualified method body by argument types", testSourceSelectsQualifiedMethodBodyByArgumentTypes),
    ("source pipeline selects qualified Float method body by argument types", testSourceSelectsQualifiedFloatMethodBodyByArgumentTypes),
    ("source pipeline selects qualified Float16 method body by argument types", testSourceSelectsQualifiedFloat16MethodBodyByArgumentTypes),
    ("source pipeline selects qualified Float32 method body by argument types", testSourceSelectsQualifiedFloat32MethodBodyByArgumentTypes),
    ("source pipeline selects qualified method body through prefix dollar", testSourceSelectsQualifiedMethodBodyThroughPrefixDollar),
    ("source pipeline accepts same-impl qualified method body references", testSourceAcceptsSameImplQualifiedMethodBodyReferences),
    ("source pipeline uses impl signatures while checking method bodies", testSourceUsesImplSignaturesWhileCheckingMethodBodies),
    ("source pipeline uses impl signatures to contextualize method body lambdas", testSourceUsesImplSignaturesToContextualizeMethodBodyLambdas),
    ("source pipeline uses binding signatures to contextualize RHS lambdas", testSourceUsesBindingSignaturesToContextualizeRhsLambdas),
    ("source pipeline accepts higher-order qualified method signature", testSourceAcceptsHigherOrderQualifiedMethodSignature),
    ("source pipeline prefers visible binding over qualified method spine", testSourcePrefersVisibleBindingOverQualifiedMethodSpine),
    ("source pipeline applies substituted qualified method signature", testSourceRejectsQualifiedMethodSignatureMismatch),
    ("source pipeline rejects qualified method dispatch with no typed candidate", testSourceRejectsQualifiedMethodDispatchWithNoTypedCandidate),
    ("source pipeline rejects qualified impl method body mismatch", testSourceRejectsQualifiedImplMethodBodyMismatch),
    ("source pipeline rejects impl method before class method metadata", testSourceRejectsImplMethodBeforeClassMethodMetadata),
    ("source pipeline rejects qualified dispatch without class method metadata", testSourceRejectsQualifiedMethodMissingClassMethod),
    ("source pipeline rejects qualified dispatch without impl method body", testSourceRejectsQualifiedMethodMissingImplBody),
    ("source pipeline rejects ambiguous qualified method bodies", testSourceRejectsAmbiguousQualifiedMethodBodies),
    ("source pipeline rejects duplicate class declarations", testSourceRejectsDuplicateClassDeclarations),
    ("source pipeline rejects duplicate concrete impl declarations", testSourceRejectsDuplicateConcreteImplDeclarations),
    ("source pipeline rejects duplicate ADT impl declarations", testSourceRejectsDuplicateAdtImplDeclarations),
    ("compiler keeps nested capability facts scoped", testSourceKeepsNestedCapabilityFactsScoped),
    ("compiler exposes imported qualified method bodies", testCompilerExposesImportedQualifiedMethodBodies),
    ("compiler hides alias-only imported capability facts in signatures", testCompilerHidesAliasOnlyImportedCapabilityFactsInSignatures),
    ("source pipeline treats capability declarations as signature separators", testSourceRejectsSignatureSeparatedByCapabilityDeclaration),
    ("source pipeline rejects separated signature", testSourceRejectsSeparatedSignature),
    ("source pipeline rejects signature name mismatch", testSourceRejectsSignatureNameMismatch),
    ("source pipeline rejects non-recursive forward reference", testSourceRejectsNonRecursiveForwardReference),
    ("source pipeline rejects retroactive rebinding recursion", testSourceRejectsRetroactiveRebindingRecursion),
    ("source pipeline accepts mutual recursion group", testSourceAcceptsMutualRecursionGroup),
    ("source pipeline rejects signature type mismatch", testSourceRejectsSignatureTypeMismatch),
    ("source pipeline accepts concrete list signature", testSourceAcceptsConcreteListSignature),
    ("source pipeline accepts nested concrete list signature", testSourceAcceptsNestedConcreteListSignature),
    ("source pipeline accepts concrete tuple signature", testSourceAcceptsConcreteTupleSignature),
    ("source pipeline accepts width-specific integer signatures", testSourceAcceptsWidthSpecificIntegerSignatures),
    ("source pipeline rejects out-of-range width-specific integer literals", testSourceRejectsOutOfRangeWidthSpecificIntegerLiterals),
    ("source pipeline rejects out-of-range width-specific branch literals", testSourceRejectsOutOfRangeWidthSpecificBranchLiterals),
    ("source pipeline rejects out-of-range width-specific literal arithmetic", testSourceRejectsOutOfRangeWidthSpecificLiteralArithmetic),
    ("source pipeline rejects out-of-range width-specific section literals", testSourceRejectsOutOfRangeWidthSpecificSectionLiterals),
    ("source pipeline accepts same-width integral operator signatures", testSourceAcceptsSameWidthIntegralOperatorSignatures),
    ("source pipeline rejects mixed-width numeric operator signatures", testSourceRejectsMixedWidthNumericOperatorSignatures),
    ("source pipeline accepts same-width float numeric operator signatures", testSourceAcceptsSameWidthFloatNumericOperatorSignatures),
    ("source pipeline keeps float signatures distinct from integer literals", testSourceRejectsFloatSignatureForIntegerLiteral),
    ("source pipeline accepts float fractional literal signatures", testSourceAcceptsFloatFractionalLiteralSignatures),
    ("source pipeline rejects integral fractional literal targets", testSourceRejectsIntegralFractionalLiteralTargets),
    ("source pipeline rejects tuple signature mismatch", testSourceRejectsTupleSignatureMismatch),
    ("source pipeline rejects tuple signature arity mismatch", testSourceRejectsTupleSignatureArityMismatch),
    ("source pipeline accepts simple function signature", testSourceAcceptsSimpleFunctionSignature),
    ("source pipeline accepts list to list function signature", testSourceAcceptsListToListFunctionSignature),
    ("source pipeline accepts parenthesized function signature", testSourceAcceptsParenthesizedFunctionSignature),
    ("source pipeline accepts right-associated chained function signature", testSourceAcceptsChainedFunctionSignature),
    ("source pipeline accepts parenthesized function override signature", testSourceAcceptsParenthesizedFunctionOverrideSignature),
    ("source pipeline accepts list of parenthesized function types", testSourceAcceptsFunctionListSignature),
    ("source pipeline accepts empty constrained signature as monomorphic", testSourceAcceptsEmptyConstrainedSignature),
    ("source pipeline accepts empty constrained tuple signature as monomorphic", testSourceAcceptsEmptyConstrainedTupleSignature),
    ("source pipeline accepts concrete constrained signature as monomorphic", testSourceAcceptsConcreteConstrainedSignature),
    ("source pipeline accepts bundled concrete constrained signature facts", testSourceAcceptsBundledConcreteConstrainedSignatureFacts),
    ("source pipeline accepts bundled width-specific numeric constrained signature facts", testSourceAcceptsBundledWidthSpecificNumericConstrainedSignatureFacts),
    ("source pipeline accepts additional concrete constrained signatures", testSourceAcceptsAdditionalConcreteConstrainedSignatures),
    ("source pipeline accepts concrete tuple constrained signature argument", testSourceAcceptsConcreteTupleConstrainedSignatureArgument),
    ("source pipeline accepts ADT application constrained signature argument", testSourceAcceptsAdtApplicationConstrainedSignatureArgument),
    ("source pipeline rejects forward capability facts for constrained signature", testSourceRejectsForwardCapabilityFactsForConstrainedSignature),
    ("source pipeline rejects concrete constrained signature without impl fact", testSourceRejectsConcreteConstrainedSignatureWithoutImplFact),
    ("source pipeline rejects unknown constrained signature constraint", testSourceRejectsUnknownConstrainedSignatureConstraint),
    ("source pipeline rejects wrong-arity constrained signature constraint", testSourceRejectsWrongArityConstrainedSignatureConstraint),
    ("source pipeline rejects type-application constrained signature argument", testSourceRejectsTypeApplicationConstrainedSignatureArgument),
    ("source pipeline rejects function constrained signature argument", testSourceRejectsFunctionConstrainedSignatureArgument),
    ("source pipeline keeps unsupported constrained signature spans on signatures", testSourceRejectsUnsupportedConstrainedSignatureSpans),
    ("source pipeline rejects list signature mismatch", testSourceRejectsListSignatureMismatch),
    ("source pipeline rejects unsupported signature surface", testSourceRejectsUnsupportedSignatureSurface),
    ("source pipeline reports duplicate constrained signature constraints", testSourceRejectsDuplicateConstrainedSignatureConstraints),
    ("source pipeline accepts variable constrained signature as monomorphic", testSourceAcceptsVariableConstrainedSignatureAsMonomorphic),
    ("source pipeline instantiates variable constrained signatures per use", testSourceInstantiatesVariableConstrainedSignaturePerUse),
    ("source pipeline instantiates primitive constrained signatures per use", testSourceInstantiatesPrimitiveConstrainedSignaturePerUse),
    ("source pipeline instantiates equality constrained signatures per use", testSourceInstantiatesEqualityConstrainedSignaturePerUse),
    ("source pipeline instantiates recursive constrained signatures per use", testSourceInstantiatesRecursiveConstrainedSignaturePerUse),
    ("source pipeline accepts unconstrained variables beside explicit constraints", testSourceAcceptsUnconstrainedVariablesBesideExplicitConstraints),
    ("source pipeline honors visible facts for variable constrained signatures", testSourceHonorsVisibleFactsForVariableConstrainedSignatures),
    ("source pipeline rejects missing use-site facts for variable constrained signatures", testSourceRejectsMissingUseSiteFactsForVariableConstrainedSignatures),
    ("source pipeline rejects ambiguous variable constrained signature use", testSourceRejectsAmbiguousVariableConstrainedSignatureUse),
    ("source pipeline preserves primitive constraints on variable constrained signatures", testSourcePreservesPrimitiveConstraintsOnVariableConstrainedSignatures),
    ("source pipeline preserves explicit constraints when primitive RHS has no quantified variables", testSourcePreservesExplicitConstraintsWhenPrimitiveRhsHasNoQuantifiedVariables),
    ("source pipeline resolves deferred constraints in impl method bodies", testSourceResolvesDeferredConstraintsInImplMethodBodies),
    ("source pipeline discards failed application argument constraints", testSourceDiscardsFailedApplicationArgumentConstraints),
    ("source pipeline discards speculative deferred constraints from recursive previews", testSourceDiscardsSpeculativeDeferredConstraintsFromRecursivePreviews),
    ("source pipeline rejects unsupported variable constrained signature contract", testSourceRejectsUnsupportedVariableConstrainedSignatureContract),
    ("source pipeline rejects unused variable constraint with bidirectional contract", testSourceRejectsUnusedVariableConstraintWithBidirectionalContract),
    ("source pipeline does not shift inference variables after rejected variable type application", testSourceRejectsVariableConstrainedTypeApplicationWithoutShiftingState),
    ("source pipeline keeps generic constructor aliases monomorphic", testSourceKeepsGenericConstructorAliasesMonomorphic),
    ("source pipeline rejects constrained signature surface with E2009", testSourceRejectsConstrainedSignatureSurface),
    ("source pipeline reports signed recursive rhs type errors", testSourceReportsSignedRecursiveRhsTypeError),
    ("signature mismatch keeps declared type for downstream checks", testSignatureMismatchKeepsDeclaredTypeDownstream),
    ("mismatched pending signature does not monomorphize following binding", testMismatchedPendingSignatureDoesNotMonomorphizeFollowingBinding)
  ]

testSignatureDirectlyAboveBinding :: IO ()
testSignatureDirectlyAboveBinding = do
  result <- compileExpr defaultWarningSettings validSignatureProgram
  assertEqual "compile errors" [] (compileErrors result)

testSignatureTypeMismatch :: IO ()
testSignatureTypeMismatch = do
  result <- compileExpr defaultWarningSettings signatureTypeMismatchProgram
  assertSingleDiagnosticCode
    "signature type mismatch error"
    "E2005"
    (compileErrors result)
  assertSingleDiagnosticPrimarySpan
    "signature type mismatch primary span"
    (SourceSpan 1 1)
    (compileErrors result)
  assertSingleDiagnosticRelatedSpan
    "signature type mismatch related span"
    (SourceSpan 2 1)
    (compileErrors result)
  assertSingleDiagnosticSubject
    "signature type mismatch subject"
    "x"
    (compileErrors result)

testSignatureSeparatedFromBinding :: IO ()
testSignatureSeparatedFromBinding = do
  result <- compileExpr defaultWarningSettings separatedSignatureProgram
  assertSingleDiagnosticContains
    "error text"
    "must be immediately followed by a matching binding"
    (compileErrors result)

testSignatureNameMismatch :: IO ()
testSignatureNameMismatch = do
  result <- compileExpr defaultWarningSettings mismatchedSignatureProgram
  assertSingleDiagnosticContains
    "error text"
    "must annotate the next binding with the same name"
    (compileErrors result)
  assertSingleDiagnosticPrimarySpan
    "signature mismatch primary span"
    (SourceSpan 1 1)
    (compileErrors result)
  assertSingleDiagnosticRelatedSpan
    "signature mismatch related span"
    (SourceSpan 2 1)
    (compileErrors result)
  assertSingleDiagnosticSubject
    "signature mismatch subject"
    "x"
    (compileErrors result)

testUseBeforeDefinition :: IO ()
testUseBeforeDefinition = do
  result <- compileExpr defaultWarningSettings useBeforeDefinitionProgram
  assertSingleDiagnosticContains
    "error text"
    "unbound variable 'x'"
    (compileErrors result)

testNestedScopeResolvesOuterBinding :: IO ()
testNestedScopeResolvesOuterBinding = do
  result <- compileExpr defaultWarningSettings nestedScopeProgram
  assertEqual "compile errors" [] (compileErrors result)

validSignatureProgram :: Expr
validSignatureProgram =
  EBlock
    [ SSignature "x" (SourceSpan 1 1) (SignatureType TypeInt),
      SLet "x" (SourceSpan 2 1) (ELit (LInt 1)),
      SExpr (SourceSpan 3 1) (EVar "x")
    ]

separatedSignatureProgram :: Expr
separatedSignatureProgram =
  EBlock
    [ SSignature "x" (SourceSpan 1 1) (SignatureType TypeInt),
      SExpr (SourceSpan 2 1) (ELit (LInt 1)),
      SLet "x" (SourceSpan 3 1) (ELit (LInt 2))
    ]

mismatchedSignatureProgram :: Expr
mismatchedSignatureProgram =
  EBlock
    [ SSignature "x" (SourceSpan 1 1) (SignatureType TypeInt),
      SLet "y" (SourceSpan 2 1) (ELit (LInt 2))
    ]

useBeforeDefinitionProgram :: Expr
useBeforeDefinitionProgram =
  EBlock
    [ SExpr (SourceSpan 1 1) (EVar "x"),
      SLet "x" (SourceSpan 2 1) (ELit (LInt 1))
    ]

nestedScopeProgram :: Expr
nestedScopeProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
      SExpr
        (SourceSpan 2 1)
        ( EBlock
            [ SExpr (SourceSpan 3 1) (EVar "x")
            ]
        )
    ]

testSelfRecursiveBinding :: IO ()
testSelfRecursiveBinding = do
  result <- compileExpr defaultWarningSettings selfRecursiveProgram
  assertEqual "compile errors" [] (compileErrors result)

selfRecursiveProgram :: Expr
selfRecursiveProgram =
  EBlock
    [ SLet "f" (SourceSpan 1 1) (EVar "f")
    ]

testMutualRecursionGroup :: IO ()
testMutualRecursionGroup = do
  result <- compileExpr defaultWarningSettings mutualRecursionProgram
  assertEqual "compile errors" [] (compileErrors result)

testThreeNodeMutualRecursionGroup :: IO ()
testThreeNodeMutualRecursionGroup = do
  result <- compileExpr defaultWarningSettings threeNodeMutualRecursionProgram
  assertEqual "compile errors" [] (compileErrors result)

testNonRecursiveForwardReference :: IO ()
testNonRecursiveForwardReference = do
  result <- compileExpr defaultWarningSettings nonRecursiveForwardReferenceProgram
  assertSingleDiagnosticContains
    "error text"
    "unbound variable 'y'"
    (compileErrors result)

testRebindingDoesNotCreateRetroactiveRecursion :: IO ()
testRebindingDoesNotCreateRetroactiveRecursion = do
  result <- compileExpr defaultWarningSettings retroactiveRebindingProgram
  assertSingleDiagnosticContains
    "error text"
    "unbound variable 'y'"
    (compileErrors result)

mutualRecursionProgram :: Expr
mutualRecursionProgram =
  EBlock
    [ SLet "even" (SourceSpan 1 1) (EVar "odd"),
      SLet "odd" (SourceSpan 2 1) (EVar "even"),
      SExpr (SourceSpan 3 1) (EVar "even")
    ]

threeNodeMutualRecursionProgram :: Expr
threeNodeMutualRecursionProgram =
  EBlock
    [ SLet "a" (SourceSpan 1 1) (EVar "b"),
      SLet "b" (SourceSpan 2 1) (EVar "c"),
      SLet "c" (SourceSpan 3 1) (EVar "a"),
      SExpr (SourceSpan 4 1) (EVar "a")
    ]

nonRecursiveForwardReferenceProgram :: Expr
nonRecursiveForwardReferenceProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (EVar "y"),
      SLet "y" (SourceSpan 2 1) (ELit (LInt 1)),
      SExpr (SourceSpan 3 1) (EVar "x")
    ]

retroactiveRebindingProgram :: Expr
retroactiveRebindingProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (EVar "y"),
      SLet "y" (SourceSpan 2 1) (ELit (LInt 1)),
      SLet "y" (SourceSpan 3 1) (EVar "x"),
      SExpr (SourceSpan 4 1) (EVar "x")
    ]

signatureTypeMismatchProgram :: Expr
signatureTypeMismatchProgram =
  EBlock
    [ SSignature "x" (SourceSpan 1 1) (SignatureType TypeInt),
      SLet "x" (SourceSpan 2 1) (ELit (LBool True))
    ]

assertSourceOk :: Text.Text -> IO ()
assertSourceOk src = do
  result <- compileSource defaultWarningSettings src
  assertEqual "compile errors" [] (compileErrors result)

assertSourceOkWithoutPrelude :: Text.Text -> IO ()
assertSourceOkWithoutPrelude src = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing src
  assertEqual "compile errors" [] (compileErrors result)

assertSourceErrorContains :: Text.Text -> Text.Text -> IO ()
assertSourceErrorContains src needle = do
  result <- compileSource defaultWarningSettings src
  assertContains "source error" needle (Text.unlines (map renderDiagnostic (compileErrors result)))

assertSourceSingleErrorContains :: Text.Text -> Text.Text -> IO ()
assertSourceSingleErrorContains src needle = do
  result <- compileSource defaultWarningSettings src
  assertSingleDiagnosticContains "source error" needle (compileErrors result)

assertSourceSingleErrorContainsWithoutPrelude :: Text.Text -> Text.Text -> IO ()
assertSourceSingleErrorContainsWithoutPrelude src needle = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing src
  assertSingleDiagnosticContains "source error" needle (compileErrors result)

assertSourceSingleErrorCodeAndPrimarySpan :: Text.Text -> Text.Text -> SourceSpan -> IO ()
assertSourceSingleErrorCodeAndPrimarySpan src expectedCode expectedSpan = do
  result <- compileSource defaultWarningSettings src
  assertSingleDiagnosticCode "source error code" expectedCode (compileErrors result)
  assertSingleDiagnosticPrimarySpan "source error primary span" expectedSpan (compileErrors result)

testSourceAcceptsSignatureAdjacency :: IO ()
testSourceAcceptsSignatureAdjacency =
  assertSourceOk "x :: Int.\nx = 1.\nx."

testSourceAcceptsCapabilityDeclarations :: IO ()
testSourceAcceptsCapabilityDeclarations =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nx :: Int.\nx = 1.\nx."

testSourceAcceptsClassMethodSignatureMetadata :: IO ()
testSourceAcceptsClassMethodSignatureMetadata =
  assertSourceOkWithoutPrelude "class Eq(a) {\nequals :: a -> a -> Bool.\nnotEquals :: a -> a -> Bool.\n}.\nimpl Eq(Int) { }.\nx :: Int.\nx = 1.\nx."

testSourceRejectsDuplicateClassMethodSignatures :: IO ()
testSourceRejectsDuplicateClassMethodSignatures =
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) { equals :: Int. equals :: Bool. }.\nx = 1." "duplicate method signature 'equals'"

testAnalyzerRejectsDuplicateClassMethodMetadata :: IO ()
testAnalyzerRejectsDuplicateClassMethodMetadata = do
  result <- compileExpr defaultWarningSettings program
  assertSingleDiagnosticContains
    "duplicate class method metadata code"
    "E1006"
    (compileErrors result)
  assertSingleDiagnosticContains
    "duplicate class method metadata summary"
    "duplicate method signature 'equals'"
    (compileErrors result)
  where
    classSpan = SourceSpan 1 1
    firstMethodSpan = SourceSpan 2 1
    secondMethodSpan = SourceSpan 3 1
    program =
      EBlock
        [ SClass
            classSpan
            "Eq"
            ["a"]
            [ ClassMethodSignature "equals" firstMethodSpan (SignatureType TypeInt),
              ClassMethodSignature "equals" secondMethodSpan (SignatureType TypeBool)
            ],
          SExpr (SourceSpan 4 1) (ELit (LInt 1))
        ]

testSourceAnalyzesImplMethodBindingMetadata :: IO ()
testSourceAnalyzesImplMethodBindingMetadata = do
  assertSourceOkWithoutPrelude "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\nx :: Int.\nx = 1.\nx."
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) {\nequals = missingImplRuntime.\n}.\nx :: Int.\nx = 1.\nx." "unbound variable 'missingImplRuntime'"

testSourceRejectsVariableTargetImplMethodBindings :: IO ()
testSourceRejectsVariableTargetImplMethodBindings =
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) { }.\nimpl Eq(a) { equals = 1. }.\nx = 1." "concrete impl target"

testSourceRejectsVariableTargetEmptyImplDeclarations :: IO ()
testSourceRejectsVariableTargetEmptyImplDeclarations =
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) { }.\nimpl Eq(a) { }.\nx = 1." "concrete impl target"

testSourceRejectsDuplicateImplMethodBindings :: IO ()
testSourceRejectsDuplicateImplMethodBindings =
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { equals = 1. equals = 2. }.\nx = 1." "duplicate method binding 'equals'"

testSourceRejectsNonBindingImplBodyItem :: IO ()
testSourceRejectsNonBindingImplBodyItem =
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { equals :: Int. }.\nx = 1." "ordinary method binding"

testSourceAcceptsSingleTargetQualifiedMethodDispatch :: IO ()
testSourceAcceptsSingleTargetQualifiedMethodDispatch =
  assertSourceOkWithoutPrelude (qualifiedEqSource <> "result :: Bool.\nresult = Eq::equals 1 1.\nresult.")

testSourceSelectsQualifiedMethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedMethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( qualifiedEqSource
        <> "impl Eq(Bool) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\nresult :: Bool.\nresult = Eq::equals True False.\nresult."
    )

testSourceSelectsQualifiedFloatMethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloatMethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\n"
        <> "impl Eq(Float) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
        <> "left :: Float.\nleft = 1.5.\n"
        <> "right :: Float.\nright = 2.25.\n"
        <> "result :: Bool.\nresult = Eq::equals left right.\nresult."
    )

testSourceSelectsQualifiedFloat16MethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloat16MethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\n"
        <> "impl Eq(Float16) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
        <> "left :: Float16.\nleft = 1.5.\n"
        <> "right :: Float16.\nright = 2.25.\n"
        <> "result :: Bool.\nresult = Eq::equals left right.\nresult."
    )

testSourceSelectsQualifiedFloat32MethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloat32MethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\n"
        <> "impl Eq(Float32) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
        <> "left :: Float32.\nleft = 1.5.\n"
        <> "right :: Float32.\nright = 2.25.\n"
        <> "result :: Bool.\nresult = Eq::equals left right.\nresult."
    )

testSourceSelectsQualifiedMethodBodyThroughPrefixDollar :: IO ()
testSourceSelectsQualifiedMethodBodyThroughPrefixDollar =
  assertSourceOkWithoutPrelude
    ( "class Choice(a) {\npick :: a -> Bool.\n}.\n"
        <> "impl Choice(Int) {\npick = \\(value) -> True.\n}.\n"
        <> "impl Choice(Bool) {\npick = \\(value) -> False.\n}.\n"
        <> "result :: Bool.\nresult = ($) Choice::pick True.\nresult."
    )

testSourceAcceptsSameImplQualifiedMethodBodyReferences :: IO ()
testSourceAcceptsSameImplQualifiedMethodBodyReferences =
  assertSourceOkWithoutPrelude
    ( "class Eq(a) {\nequals :: a -> a -> Bool.\nnotEquals :: a -> a -> Bool.\n}.\n"
        <> "impl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\nnotEquals = \\(left) -> \\(right) -> Eq::equals left right != True.\n}.\n"
        <> "result :: Bool.\nresult = Eq::notEquals 1 2.\nresult."
    )

testSourceUsesImplSignaturesWhileCheckingMethodBodies :: IO ()
testSourceUsesImplSignaturesWhileCheckingMethodBodies =
  assertSourceOkWithoutPrelude
    ( "class Check(a) {\ncheck :: a -> Bool.\nnotCheck :: a -> Bool.\n}.\n"
        <> "impl Check(Int) {\ncheck = \\(value) -> True.\nnotCheck = \\(value) -> Check::check value != True.\n}.\n"
        <> "impl Check(Bool) {\ncheck = \\(value) -> False.\nnotCheck = \\(value) -> Check::check value != True.\n}.\n"
        <> "result :: Bool.\nresult = Check::notCheck 1.\nresult."
    )

testSourceUsesImplSignaturesToContextualizeMethodBodyLambdas :: IO ()
testSourceUsesImplSignaturesToContextualizeMethodBodyLambdas =
  assertSourceOkWithoutPrelude
    ( "class D(a) {\nn :: a -> Bool.\n}.\n"
        <> "impl D(Int) {\nn = \\(value) -> True.\n}.\n"
        <> "impl D(Bool) {\nn = \\(value) -> False.\n}.\n"
        <> "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int) {\nm = \\(x) -> D::n x.\n}.\n"
        <> "result :: Bool.\nresult = C::m 1.\nresult."
    )

testSourceUsesBindingSignaturesToContextualizeRhsLambdas :: IO ()
testSourceUsesBindingSignaturesToContextualizeRhsLambdas =
  assertSourceOkWithoutPrelude
    ( "class D(a) {\nn :: a -> Bool.\n}.\n"
        <> "impl D(Int) {\nn = \\(value) -> True.\n}.\n"
        <> "impl D(Bool) {\nn = \\(value) -> False.\n}.\n"
        <> "f :: Int -> Bool.\n"
        <> "f = \\(x) -> D::n x.\n"
        <> "result :: Bool.\nresult = f 1.\nresult."
    )

testSourceAcceptsHigherOrderQualifiedMethodSignature :: IO ()
testSourceAcceptsHigherOrderQualifiedMethodSignature =
  assertSourceOkWithoutPrelude
    ( "class Apply(a) {\napply :: (Int -> Int) -> Int.\n}.\n"
        <> "impl Apply(Int) {\napply = \\(f) -> f 1.\n}.\n"
        <> "result :: Int.\nresult = Apply::apply (+ 1).\nresult."
    )

testSourcePrefersVisibleBindingOverQualifiedMethodSpine :: IO ()
testSourcePrefersVisibleBindingOverQualifiedMethodSpine = do
  result <-
    compileExpr
      defaultWarningSettings
      ( EBlock
          [ SClass (SourceSpan 1 1) "Eq" ["a"] [],
            SLet "Eq::helper" (SourceSpan 2 1) (ELambda "value" (EVar "value")),
            SExpr (SourceSpan 3 1) (EApply (EVar "Eq::helper") (ELit (LInt 1)))
          ]
      )
  assertEqual "binding-precedence compile errors" [] (compileErrors result)

testSourceRejectsQualifiedMethodSignatureMismatch :: IO ()
testSourceRejectsQualifiedMethodSignatureMismatch =
  assertSourceSingleErrorContainsWithoutPrelude
    (qualifiedEqSource <> "result = Eq::equals 1 True.\nresult.")
    "cannot apply function of type Int -> Bool to argument of type Bool"

testSourceRejectsQualifiedMethodDispatchWithNoTypedCandidate :: IO ()
testSourceRejectsQualifiedMethodDispatchWithNoTypedCandidate =
  assertSourceSingleErrorContainsWithoutPrelude
    ( qualifiedEqSource
        <> "impl Eq(Bool) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\nresult = Eq::equals 1 False.\nresult."
    )
    "no matching qualified method body 'Eq::equals' for argument types Int, Bool"

testSourceRejectsQualifiedImplMethodBodyMismatch :: IO ()
testSourceRejectsQualifiedImplMethodBodyMismatch =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) {\nequals = 1.\n}.\nresult = Eq::equals 1 1.\nresult."
    "impl method 'Eq::equals' declared as Int -> Int -> Bool but inferred as Int"

testSourceRejectsImplMethodBeforeClassMethodMetadata :: IO ()
testSourceRejectsImplMethodBeforeClassMethodMetadata = do
  result <-
    compileSourceWithPrelude
      defaultWarningSettings
      Nothing
      "impl Eq(Int) {\nequals = 1.\n}.\nclass Eq(a) {\nequals :: a -> a -> Bool.\n}.\nresult = Eq::equals 1 1.\nresult."
  assertSingleDiagnosticContains
    "impl-before-class method metadata"
    "class method metadata for 'Eq::equals' must be declared before impl method body"
    (compileErrors result)
  assertSingleDiagnosticPrimarySpan
    "impl-before-class method metadata span"
    (SourceSpan 2 1)
    (compileErrors result)

testSourceRejectsQualifiedMethodMissingClassMethod :: IO ()
testSourceRejectsQualifiedMethodMissingClassMethod =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) { }.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\nresult = Eq::equals 1 1.\nresult."
    "class method metadata for 'Eq::equals' must be declared before impl method body"

testSourceRejectsQualifiedMethodMissingImplBody :: IO ()
testSourceRejectsQualifiedMethodMissingImplBody =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) { }.\nresult = Eq::equals 1 1.\nresult."
    "missing impl method body 'Eq::equals'"

testSourceRejectsAmbiguousQualifiedMethodBodies :: IO ()
testSourceRejectsAmbiguousQualifiedMethodBodies =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Classify(a) {\nclassify :: Int -> Bool.\n}.\nimpl Classify(Int) {\nclassify = \\(value) -> value == 1.\n}.\nimpl Classify(Bool) {\nclassify = \\(value) -> value == 2.\n}.\nresult = Classify::classify 1.\nresult."
    "ambiguous qualified method body 'Classify::classify' for argument types Int"

qualifiedEqSource :: Text.Text
qualifiedEqSource =
  "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"

testSourceRejectsDuplicateClassDeclarations :: IO ()
testSourceRejectsDuplicateClassDeclarations =
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) { }.\nclass Eq(b) { }.\nx = 1." "E1004"

testSourceRejectsDuplicateConcreteImplDeclarations :: IO ()
testSourceRejectsDuplicateConcreteImplDeclarations =
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nimpl Eq(Int) { }.\nx = 1." "E1005"

testSourceRejectsDuplicateAdtImplDeclarations :: IO ()
testSourceRejectsDuplicateAdtImplDeclarations = do
  assertSourceSingleErrorContainsWithoutPrelude "data Color = Red.\nclass Eq(a) { }.\nimpl Eq(Color) { }.\nimpl Eq(Color) { }.\nx = 1." "E1005"
  assertSourceSingleErrorContainsWithoutPrelude "data Box a = Box a.\nclass Eq(a) { }.\nimpl Eq(Box(Int)) { }.\nimpl Eq(Box(Int)) { }.\nx = 1." "E1005"

testSourceKeepsNestedCapabilityFactsScoped :: IO ()
testSourceKeepsNestedCapabilityFactsScoped = do
  result <- compileExpr defaultWarningSettings program
  assertSingleDiagnosticContains
    "nested capability fact isolation"
    "missing class declaration 'Eq'"
    (compileErrors result)
  where
    spanValue = SourceSpan 1 1
    eqInt = ConstraintTypeName "Int"
    program =
      EBlock
        [ SLet
            "seed"
            spanValue
            ( EBlock
                [ SClass spanValue "Eq" ["a"] [],
                  SImpl spanValue "Eq" [eqInt] [],
                  SExpr spanValue (ELit (LInt 0))
                ]
            ),
          SSignature "x" spanValue (ConstrainedSignature [SignatureConstraint "Eq" [eqInt]] eqInt),
          SLet "x" spanValue (ELit (LInt 1))
        ]

testCompilerExposesImportedQualifiedMethodBodies :: IO ()
testCompilerExposesImportedQualifiedMethodBodies = do
  result <- compileExpr defaultWarningSettings importedQualifiedMethodFactsProgram
  assertEqual "imported qualified method compile errors" [] (compileErrors result)

testCompilerHidesAliasOnlyImportedCapabilityFactsInSignatures :: IO ()
testCompilerHidesAliasOnlyImportedCapabilityFactsInSignatures = do
  result <- compileExpr defaultWarningSettings aliasOnlyImportedCapabilityFactsProgram
  assertSingleDiagnosticContains
    "alias-only capability fact isolation"
    "missing class declaration 'RemoteEq'"
    (compileErrors result)

importedQualifiedMethodFactsProgram :: Expr
importedQualifiedMethodFactsProgram =
  EBlock
    [ SModule (SourceSpan 1 1) ["Lib"],
      SClass
        (SourceSpan 2 1)
        "RemoteEq"
        ["a"]
        [ ClassMethodSignature
            "equals"
            (SourceSpan 3 1)
            ( ConstrainedSignature
                []
                ( ConstraintTypeFunction
                    (ConstraintTypeName "a")
                    (ConstraintTypeFunction (ConstraintTypeName "a") (ConstraintTypeName "Bool"))
                )
            )
        ],
      SImpl
        (SourceSpan 4 1)
        "RemoteEq"
        [ConstraintTypeName "Int"]
        [ ImplMethod
            "equals"
            (SourceSpan 5 1)
            (ELambda "left" (ELambda "right" (EBinary "==" (EVar "left") (EVar "right"))))
        ],
      SModule (SourceSpan 6 1) ["App"],
      SImport (SourceSpan 7 1) ["Lib"] Nothing Nothing,
      SExpr
        (SourceSpan 9 1)
        ( EApply
            (EApply (EVar "RemoteEq::equals") (ELit (LInt 1)))
            (ELit (LInt 1))
        )
    ]

aliasOnlyImportedCapabilityFactsProgram :: Expr
aliasOnlyImportedCapabilityFactsProgram =
  EBlock
    [ SModule (SourceSpan 1 1) ["Lib"],
      SClass (SourceSpan 2 1) "RemoteEq" ["a"] [],
      SImpl (SourceSpan 3 1) "RemoteEq" [ConstraintTypeName "Int"] [],
      SModule (SourceSpan 4 1) ["App"],
      SImport (SourceSpan 5 1) ["Lib"] (Just "Lib") Nothing,
      SSignature
        "x"
        (SourceSpan 6 1)
        (ConstrainedSignature [SignatureConstraint "RemoteEq" [ConstraintTypeName "Int"]] (ConstraintTypeName "Int")),
      SLet "x" (SourceSpan 7 1) (ELit (LInt 1))
    ]

testSourceRejectsSignatureSeparatedByCapabilityDeclaration :: IO ()
testSourceRejectsSignatureSeparatedByCapabilityDeclaration =
  assertSourceErrorContains "x :: Int.\nclass Eq(a) { }.\nx = 1." "E1002"

testSourceRejectsSeparatedSignature :: IO ()
testSourceRejectsSeparatedSignature =
  assertSourceErrorContains "x :: Int.\n1.\nx = 2." "E1002"

testSourceRejectsSignatureNameMismatch :: IO ()
testSourceRejectsSignatureNameMismatch =
  assertSourceErrorContains "x :: Int.\ny = 2." "E1003"

testSourceRejectsNonRecursiveForwardReference :: IO ()
testSourceRejectsNonRecursiveForwardReference =
  assertSourceErrorContains "x = y.\ny = 1.\nx." "E1001"

testSourceRejectsRetroactiveRebindingRecursion :: IO ()
testSourceRejectsRetroactiveRebindingRecursion =
  assertSourceErrorContains "x = y.\ny = 1.\ny = x.\nx." "E1001"

testSourceAcceptsMutualRecursionGroup :: IO ()
testSourceAcceptsMutualRecursionGroup =
  assertSourceOk "even = odd.\nodd = even.\neven."

testSourceRejectsSignatureTypeMismatch :: IO ()
testSourceRejectsSignatureTypeMismatch = do
  result <- compileSource defaultWarningSettings "x :: Int.\nx = True."
  assertSingleDiagnosticCode
    "source signature type mismatch code"
    "E2005"
    (compileErrors result)
  assertSingleDiagnosticPrimarySpan
    "source signature type mismatch primary span"
    (SourceSpan 1 1)
    (compileErrors result)
  assertSingleDiagnosticRelatedSpan
    "source signature type mismatch related span"
    (SourceSpan 2 1)
    (compileErrors result)
  assertSingleDiagnosticSubject
    "source signature type mismatch subject"
    "x"
    (compileErrors result)

testSourceAcceptsConcreteListSignature :: IO ()
testSourceAcceptsConcreteListSignature =
  assertSourceOk "x :: [Int].\nx = [1]."

testSourceAcceptsNestedConcreteListSignature :: IO ()
testSourceAcceptsNestedConcreteListSignature =
  assertSourceOk "x :: [[Bool]].\nx = [[True], [False]]."

testSourceAcceptsConcreteTupleSignature :: IO ()
testSourceAcceptsConcreteTupleSignature =
  assertSourceOk "pair :: (Int, Bool).\npair = (1, True).\npair."

testSourceAcceptsWidthSpecificIntegerSignatures :: IO ()
testSourceAcceptsWidthSpecificIntegerSignatures = do
  assertSourceOk "x :: Int8.\nx = 1."
  assertSourceOk "x :: Int8.\nx = 127."
  assertSourceOk "x :: UInt8.\nx = 255."
  assertSourceOk "x :: UInt64.\nx = 1."
  assertSourceOk "x :: UInt64.\nx = 18446744073709551615."
  assertSourceOk "xs :: [Int32].\nxs = [1, 2, 3]."
  assertSourceOkWithoutPrelude "class Num(a) { }.\nimpl Num(UInt16) { }.\nx :: @{Num(UInt16)}: UInt16.\nx = 1."

testSourceRejectsOutOfRangeWidthSpecificIntegerLiterals :: IO ()
testSourceRejectsOutOfRangeWidthSpecificIntegerLiterals = do
  assertSourceSingleErrorContains "x :: UInt8.\nx = 300." "E2005"
  assertSourceSingleErrorContains "x :: Int8.\nx = 128." "E2005"
  assertSourceSingleErrorContains "x :: UInt64.\nx = 18446744073709551616." "E2005"
  assertSourceSingleErrorContains "xs :: [UInt8].\nxs = [1, 300]." "E2005"

testSourceRejectsOutOfRangeWidthSpecificBranchLiterals :: IO ()
testSourceRejectsOutOfRangeWidthSpecificBranchLiterals = do
  assertSourceSingleErrorContains "x :: UInt8.\nx = if True 1 else 300." "E2005"
  assertSourceSingleErrorContains "x :: UInt8.\nx = case 0 { | 0 -> 1 | _ -> 300 }." "E2005"
  assertSourceSingleErrorContains "x :: (UInt8, UInt8).\nx = if True (1, 1) else (2, 300)." "E2005"
  assertSourceSingleErrorContains "f :: UInt8 -> UInt8.\nf = if True (\\(x) -> 1) else (\\(x) -> 300)." "E2005"

testSourceRejectsOutOfRangeWidthSpecificLiteralArithmetic :: IO ()
testSourceRejectsOutOfRangeWidthSpecificLiteralArithmetic = do
  assertSourceSingleErrorContains "x :: UInt8.\nx = 1 + 300." "E2005"
  assertSourceSingleErrorContains "x :: UInt8.\nx = 200 + 100." "E2005"
  assertSourceSingleErrorContains "x :: UInt8.\nx = 0 - 1." "E2005"
  assertSourceSingleErrorContains "x :: UInt8.\nx = 16 * 16." "E2005"

testSourceRejectsOutOfRangeWidthSpecificSectionLiterals :: IO ()
testSourceRejectsOutOfRangeWidthSpecificSectionLiterals = do
  assertSourceSingleErrorContains "inc :: UInt8 -> UInt8.\ninc = (+ 300)." "E2005"
  assertSourceSingleErrorContains "inc :: UInt8 -> UInt8.\ninc = (300 +)." "E2005"

testSourceAcceptsSameWidthIntegralOperatorSignatures :: IO ()
testSourceAcceptsSameWidthIntegralOperatorSignatures = do
  assertSourceOk "add :: Int8 -> Int8 -> Int8.\nadd = (+)."
  assertSourceOk "lt :: UInt32 -> UInt32 -> Bool.\nlt = (<)."

testSourceRejectsMixedWidthNumericOperatorSignatures :: IO ()
testSourceRejectsMixedWidthNumericOperatorSignatures =
  assertSourceSingleErrorContains "add :: Int8 -> UInt8 -> Int8.\nadd = (+)." "E2005"

testSourceAcceptsSameWidthFloatNumericOperatorSignatures :: IO ()
testSourceAcceptsSameWidthFloatNumericOperatorSignatures = do
  assertSourceOk "fadd :: Float -> Float -> Float.\nfadd = (+)."
  assertSourceOk "fadd64 :: Float64 -> Float64 -> Float64.\nfadd64 = (+)."

testSourceRejectsFloatSignatureForIntegerLiteral :: IO ()
testSourceRejectsFloatSignatureForIntegerLiteral =
  assertSourceSingleErrorContains "x :: Float64.\nx = 1." "E2005"

testSourceAcceptsFloatFractionalLiteralSignatures :: IO ()
testSourceAcceptsFloatFractionalLiteralSignatures = do
  assertSourceOk "x :: Float.\nx = 1.5."
  assertSourceOk "x :: Float16.\nx = 1.5."
  assertSourceOk "x :: Float32.\nx = 1.5."
  assertSourceOk "x :: Float64.\nx = 1.5."
  assertSourceOk "xs :: [Float64].\nxs = [1.5, 2.25]."

testSourceRejectsIntegralFractionalLiteralTargets :: IO ()
testSourceRejectsIntegralFractionalLiteralTargets = do
  assertSourceSingleErrorContains "x :: Int.\nx = 1.5." "E2005"

testSourceRejectsTupleSignatureMismatch :: IO ()
testSourceRejectsTupleSignatureMismatch = do
  result <- compileSource defaultWarningSettings "pair :: (Int, Bool).\npair = (1, 2)."
  assertSingleDiagnosticCode
    "source tuple signature mismatch code"
    "E2005"
    (compileErrors result)

testSourceRejectsTupleSignatureArityMismatch :: IO ()
testSourceRejectsTupleSignatureArityMismatch = do
  result <- compileSource defaultWarningSettings "pair :: (Int, Bool).\npair = (1, True, 3)."
  assertSingleDiagnosticCode
    "source tuple signature arity mismatch code"
    "E2005"
    (compileErrors result)

testSourceAcceptsSimpleFunctionSignature :: IO ()
testSourceAcceptsSimpleFunctionSignature =
  assertSourceOk "f :: Int -> Int.\nf = (+ 1)."

testSourceAcceptsListToListFunctionSignature :: IO ()
testSourceAcceptsListToListFunctionSignature =
  assertSourceOk "f :: [Int] -> [Int].\nf = filter (> 1)."

testSourceAcceptsParenthesizedFunctionSignature :: IO ()
testSourceAcceptsParenthesizedFunctionSignature =
  assertSourceOk "f :: ([Int]) -> ([Int]).\nf = filter (> 1)."

testSourceAcceptsChainedFunctionSignature :: IO ()
testSourceAcceptsChainedFunctionSignature =
  assertSourceOk "f :: Int -> Int -> Int.\nf = (+)."

testSourceAcceptsParenthesizedFunctionOverrideSignature :: IO ()
testSourceAcceptsParenthesizedFunctionOverrideSignature =
  assertSourceOk "applyToOne :: (Int -> Int) -> Int.\napplyToOne = \\(f) -> f 1."

testSourceAcceptsFunctionListSignature :: IO ()
testSourceAcceptsFunctionListSignature =
  assertSourceOk "fns :: [(Int -> Int)].\nfns = [(+ 1)]."

testSourceAcceptsEmptyConstrainedSignature :: IO ()
testSourceAcceptsEmptyConstrainedSignature =
  assertSourceOk "applyToOne :: @{}: (Int -> Int) -> Int.\napplyToOne = \\(f) -> f 1."

testSourceAcceptsEmptyConstrainedTupleSignature :: IO ()
testSourceAcceptsEmptyConstrainedTupleSignature =
  assertSourceOk "pair :: @{}: (Int, Bool).\npair = (1, True).\npair."

testSourceAcceptsConcreteConstrainedSignature :: IO ()
testSourceAcceptsConcreteConstrainedSignature =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nx :: @{Eq(Int)}: Int.\nx = 1."

testSourceAcceptsBundledConcreteConstrainedSignatureFacts :: IO ()
testSourceAcceptsBundledConcreteConstrainedSignatureFacts =
  assertSourceOk "x :: @{Eq(Int)}: Int.\nx = 1."

testSourceAcceptsBundledWidthSpecificNumericConstrainedSignatureFacts :: IO ()
testSourceAcceptsBundledWidthSpecificNumericConstrainedSignatureFacts = do
  assertSourceOk "x :: @{Num(UInt16)}: UInt16.\nx = 1."
  assertSourceOk "x :: @{Integral(Int32)}: Int32.\nx = 1."
  assertSourceOk "x :: @{Fractional(Float32)}: Float32.\nx = toFloat32 1."
  assertSourceOk "x :: @{Showable(Float64)}: Float64.\nx = toFloat64 1."

testSourceAcceptsAdditionalConcreteConstrainedSignatures :: IO ()
testSourceAcceptsAdditionalConcreteConstrainedSignatures = do
  assertSourceOkWithoutPrelude "class Default(a) { }.\nimpl Default(Bool) { }.\nx :: @{Default(Bool)}: Bool.\nx = True."
  assertSourceOkWithoutPrelude "class Fractional(a) { }.\nimpl Fractional(Int) { }.\nx :: @{Fractional(Int)}: Int.\nx = 1."
  assertSourceOkWithoutPrelude "class Integral(a) { }.\nimpl Integral(Int) { }.\nx :: @{Integral(Int)}: Int.\nx = 1."
  assertSourceOkWithoutPrelude "class Num(a) { }.\nimpl Num(Int) { }.\nx :: @{Num(Int)}: Int.\nx = 1."
  assertSourceOkWithoutPrelude "class Ord(a) { }.\nimpl Ord(Int) { }.\nx :: @{Ord(Int)}: Int.\nx = 1."
  assertSourceOkWithoutPrelude "class Showable(a) { }.\nimpl Showable([[Bool]]) { }.\nx :: @{Showable([[Bool]])}: [[Bool]].\nx = [[True], [False]]."

testSourceAcceptsConcreteTupleConstrainedSignatureArgument :: IO ()
testSourceAcceptsConcreteTupleConstrainedSignatureArgument =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq((Int, Bool)) { }.\npair :: @{Eq((Int, Bool))}: (Int, Bool).\npair = (1, True)."

testSourceAcceptsAdtApplicationConstrainedSignatureArgument :: IO ()
testSourceAcceptsAdtApplicationConstrainedSignatureArgument =
  assertSourceOkWithoutPrelude "data Box a = Box a.\nclass Eq(a) { }.\nimpl Eq(Box(Int)) { }.\nx :: @{Eq(Box(Int))}: Int.\nx = 1."

testSourceRejectsForwardCapabilityFactsForConstrainedSignature :: IO ()
testSourceRejectsForwardCapabilityFactsForConstrainedSignature =
  assertSourceSingleErrorContainsWithoutPrelude "x :: @{Eq(Int)}: Int.\nx = 1.\nclass Eq(a) { }.\nimpl Eq(Int) { }." "missing class declaration 'Eq'"

testSourceRejectsConcreteConstrainedSignatureWithoutImplFact :: IO ()
testSourceRejectsConcreteConstrainedSignatureWithoutImplFact =
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) { }.\nx :: @{Eq(Int)}: Int.\nx = 1." "missing impl fact 'Eq(Int)'"

testSourceRejectsUnknownConstrainedSignatureConstraint :: IO ()
testSourceRejectsUnknownConstrainedSignatureConstraint =
  assertSourceSingleErrorContains "x :: @{Unknown(Int)}: Int.\nx = 1." "E2009"

testSourceRejectsWrongArityConstrainedSignatureConstraint :: IO ()
testSourceRejectsWrongArityConstrainedSignatureConstraint =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) { }.\nimpl Eq(Int) { }.\nx :: @{Eq(Int, Bool)}: Int.\nx = 1."
    "constraint 'Eq' expects 1 argument(s), got 2"

testSourceRejectsTypeApplicationConstrainedSignatureArgument :: IO ()
testSourceRejectsTypeApplicationConstrainedSignatureArgument =
  assertSourceSingleErrorContains "x :: @{Eq(Maybe(Int))}: Int.\nx = 1." "E2009"

testSourceRejectsFunctionConstrainedSignatureArgument :: IO ()
testSourceRejectsFunctionConstrainedSignatureArgument =
  assertSourceSingleErrorContains "x :: @{Eq(Int -> Int)}: Int.\nx = 1." "E2009"

testSourceRejectsUnsupportedConstrainedSignatureSpans :: IO ()
testSourceRejectsUnsupportedConstrainedSignatureSpans = do
  let assertSignatureSpan signatureSource =
        assertSourceSingleErrorCodeAndPrimarySpan
          ("prefix = 0.\n" <> signatureSource <> "\n")
          "E2009"
          (SourceSpan 2 1)
  assertSignatureSpan "x :: @{Unknown(Int)}: Int.\nx = 1."
  assertSignatureSpan "x :: @{Eq(Int, Bool)}: Int.\nx = 1."
  assertSignatureSpan "x :: @{Eq(Maybe(Int))}: Int.\nx = 1."
  assertSignatureSpan "x :: @{Eq(Int -> Int)}: Int.\nx = 1."
  assertSignatureSpan "f :: @{Eq(a), Eq(a)}: a -> a.\nf = \\(x) -> x."
  assertSignatureSpan "f :: @{Eq(a)}: Int -> Int.\nf = \\(x) -> x."

testSourceRejectsListSignatureMismatch :: IO ()
testSourceRejectsListSignatureMismatch = do
  result <- compileSource defaultWarningSettings "x :: [Bool].\nx = [1]."
  assertSingleDiagnosticCode
    "source list signature mismatch code"
    "E2005"
    (compileErrors result)

testSourceRejectsUnsupportedSignatureSurface :: IO ()
testSourceRejectsUnsupportedSignatureSurface =
  assertSourceSingleErrorContains "x :: [a].\nx = [1]." "E2009"

testSourceRejectsDuplicateConstrainedSignatureConstraints :: IO ()
testSourceRejectsDuplicateConstrainedSignatureConstraints = do
  result <- compileSource defaultWarningSettings "f :: @{Eq(a), Eq(a)}: a -> a.\nf = \\(x) -> x."
  assertSingleDiagnosticCode
    "source duplicate constrained signature code"
    "E2009"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source duplicate constrained signature text"
    "duplicate constraint 'Eq'"
    (compileErrors result)

testSourceAcceptsVariableConstrainedSignatureAsMonomorphic :: IO ()
testSourceAcceptsVariableConstrainedSignatureAsMonomorphic =
  assertSourceOk "id :: @{Eq(a)}: a -> a.\nid = \\(x) -> x.\nid 1."

testSourceInstantiatesVariableConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesVariableConstrainedSignaturePerUse =
  assertSourceOk "id :: @{Eq(a)}: a -> a.\nid = \\(x) -> x.\nx = id 1.\ny = id True."

testSourceInstantiatesPrimitiveConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesPrimitiveConstrainedSignaturePerUse =
  assertSourceOkWithoutPrelude "class Num(a) { }.\nimpl Num(Int32) { }.\nimpl Num(Int64) { }.\nadd :: @{Num(a)}: a -> a -> a.\nadd = \\(x) -> \\(y) -> x + y.\na32 :: Int32.\na32 = 1.\nb32 :: Int32.\nb32 = 2.\nsmall = add a32 b32.\na64 :: Int64.\na64 = 3.\nb64 :: Int64.\nb64 = 4.\nwide = add a64 b64."

testSourceInstantiatesEqualityConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesEqualityConstrainedSignaturePerUse =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nimpl Eq(Bool) { }.\nsame :: @{Eq(a)}: a -> a -> Bool.\nsame = \\(x) -> \\(y) -> x == y.\nintValue = same 1 2.\nboolValue = same True False."

testSourceInstantiatesRecursiveConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesRecursiveConstrainedSignaturePerUse =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nimpl Eq(Bool) { }.\nchoose :: @{Eq(a)}: a -> a.\nchoose = if True \\(x) -> x else choose.\nintValue = choose 1.\nboolValue = choose True."

testSourceAcceptsUnconstrainedVariablesBesideExplicitConstraints :: IO ()
testSourceAcceptsUnconstrainedVariablesBesideExplicitConstraints =
  assertSourceOk "choose :: @{Eq(a)}: a -> b -> a.\nchoose = \\(x) -> \\(y) -> x.\nintBool = choose 1 True.\nintInt = choose 2 3."

testSourceHonorsVisibleFactsForVariableConstrainedSignatures :: IO ()
testSourceHonorsVisibleFactsForVariableConstrainedSignatures =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nimpl Eq(Bool) { }.\nid :: @{Eq(a)}: a -> a.\nid = \\(x) -> x.\nx = id 1.\ny = id True."

testSourceRejectsMissingUseSiteFactsForVariableConstrainedSignatures :: IO ()
testSourceRejectsMissingUseSiteFactsForVariableConstrainedSignatures =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) { }.\nimpl Eq(Int) { }.\nid :: @{Eq(a)}: a -> a.\nid = \\(x) -> x.\nok = id 1.\nbad = id True."
    "missing impl fact 'Eq(Bool)'"

testSourceRejectsAmbiguousVariableConstrainedSignatureUse :: IO ()
testSourceRejectsAmbiguousVariableConstrainedSignatureUse =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) { }.\nimpl Eq(Int) { }.\nid :: @{Eq(a)}: a -> a.\nid = \\(x) -> x.\nambiguous = id []."
    "ambiguous/defaulting explicit constraint"

testSourcePreservesPrimitiveConstraintsOnVariableConstrainedSignatures :: IO ()
testSourcePreservesPrimitiveConstraintsOnVariableConstrainedSignatures =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Showable(a) { }.\nimpl Showable(Bool) { }.\naddSelf :: @{Showable(a)}: a -> a.\naddSelf = \\(x) -> x + x.\nbad = addSelf True."
    "cannot apply function"

testSourcePreservesExplicitConstraintsWhenPrimitiveRhsHasNoQuantifiedVariables :: IO ()
testSourcePreservesExplicitConstraintsWhenPrimitiveRhsHasNoQuantifiedVariables =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Showable(a) { }.\naddSelf :: @{Showable(a)}: a -> a.\naddSelf = \\(x) -> x + x.\ngood = addSelf 1."
    "missing impl fact 'Showable(Int)'"

testSourceResolvesDeferredConstraintsInImplMethodBodies :: IO ()
testSourceResolvesDeferredConstraintsInImplMethodBodies =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) { }.\nimpl Eq(Int) { }.\nid :: @{Eq(a)}: a -> a.\nid = \\(x) -> x.\nclass Use(a) { use :: a -> a. }.\nimpl Use(Bool) { use = id. }.\nvalue = 1."
    "missing impl fact 'Eq(Bool)'"

testSourceDiscardsFailedApplicationArgumentConstraints :: IO ()
testSourceDiscardsFailedApplicationArgumentConstraints = do
  result <-
    compileSourceWithPrelude
      defaultWarningSettings
      Nothing
      "class Need(a) { }.\nimpl Need(Int) { }.\nuse :: @{Need(a)}: a -> a.\nuse = \\(x) -> x.\nbad = 1 (use True)."
  assertSingleDiagnosticContains
    "failed application only reports apply error"
    "cannot apply function of type Int to argument of type Bool"
    (compileErrors result)

testSourceDiscardsSpeculativeDeferredConstraintsFromRecursivePreviews :: IO ()
testSourceDiscardsSpeculativeDeferredConstraintsFromRecursivePreviews = do
  result <- compileExpr defaultWarningSettings speculativePreviewDeferredConstraintProgram
  assertEqual "compile errors" [] (compileErrors result)

speculativePreviewDeferredConstraintProgram :: Expr
speculativePreviewDeferredConstraintProgram =
  EBlock
    [ SModule (SourceSpan 1 1) ["Base"],
      SClass (SourceSpan 2 1) "Eq" ["a"] [],
      SImpl (SourceSpan 3 1) "Eq" [ConstraintTypeName "Int"] [],
      SModule (SourceSpan 4 1) ["Facts"],
      SImport (SourceSpan 5 1) ["Base"] Nothing Nothing,
      SImpl (SourceSpan 6 1) "Eq" [ConstraintTypeName "Bool"] [],
      SModule (SourceSpan 7 1) ["Main"],
      SImport (SourceSpan 8 1) ["Base"] Nothing Nothing,
      SSignature
        "id"
        (SourceSpan 9 1)
        ( ConstrainedSignature
            [SignatureConstraint "Eq" [ConstraintTypeName "a"]]
            (ConstraintTypeFunction (ConstraintTypeName "a") (ConstraintTypeName "a"))
        ),
      SLet "id" (SourceSpan 10 1) (ELambda "x" (EVar "x")),
      SLet "value" (SourceSpan 11 1) speculativePreviewDeferredConstraintBlock,
      SExpr (SourceSpan 18 1) (EVar "value")
    ]

speculativePreviewDeferredConstraintBlock :: Expr
speculativePreviewDeferredConstraintBlock =
  EBlock
    [ SLet
        "left"
        (SourceSpan 12 1)
        ( EIf
            (ELit (LBool True))
            (ELambda "x" (EVar "x"))
            (ELambda "x" (EVar "right"))
        ),
      SLet "early" (SourceSpan 13 1) (EApply (EVar "left") (ELit (LBool True))),
      SImport (SourceSpan 14 1) ["Facts"] Nothing Nothing,
      SLet
        "right"
        (SourceSpan 15 1)
        ( EIf
            (ELit (LBool False))
            (EApply (EVar "left") (ELit (LBool True)))
            (EApply (EVar "id") (ELit (LBool True)))
        ),
      SExpr (SourceSpan 16 1) (EVar "early")
    ]

testSourceInstantiatesOrdinaryBindingSchemesPerUse :: IO ()
testSourceInstantiatesOrdinaryBindingSchemesPerUse =
  assertSourceOk "id = \\(x) -> x.\nintValue = id 1.\nboolValue = id True."

testSourceInstantiatesUnconstrainedNumericBindingVariablesPerUse :: IO ()
testSourceInstantiatesUnconstrainedNumericBindingVariablesPerUse =
  assertSourceOk "f = \\(x) -> \\(y) -> (x + x, y).\na = f 1 True.\nb = f 2 3."

testSourceInstantiatesUnconstrainedEqualityBindingVariablesPerUse :: IO ()
testSourceInstantiatesUnconstrainedEqualityBindingVariablesPerUse =
  assertSourceOk "f = \\(x) -> \\(y) -> (x == x, y).\na = f 1 True.\nb = f 2 3."

testSourceInstantiatesRecursiveBindingSchemesPerUse :: IO ()
testSourceInstantiatesRecursiveBindingSchemesPerUse =
  assertSourceOk "choose = if True \\(x) -> x else choose.\nintValue = choose 1.\nboolValue = choose True."

testSourceInstantiatesMutualRecursiveBindingSchemesPerUse :: IO ()
testSourceInstantiatesMutualRecursiveBindingSchemesPerUse =
  assertSourceOk "left = if True \\(x) -> x else right.\nright = if False \\(x) -> x else left.\nintValue = left 1.\nboolValue = right True."

testSourceInstantiatesInterleavedMutualRecursiveBindingSchemesPerUse :: IO ()
testSourceInstantiatesInterleavedMutualRecursiveBindingSchemesPerUse =
  assertSourceOk "left = if True \\(x) -> x else right.\nintValue = left 1.\nright = if False \\(x) -> x else left.\nboolValue = right True."

testSourceKeepsLaterRebindingOverRecursiveScheme :: IO ()
testSourceKeepsLaterRebindingOverRecursiveScheme =
  assertSourceSingleErrorContains
    "left = if True \\(x) -> x else right.\nright = if False \\(x) -> x else left.\nleft = \\(x) -> x + 1.\nbad = left True."
    "cannot apply function of type Int -> Int to argument of type Bool"

testSourceRejectsInterleavedUseConstrainedByLaterRecursiveMember :: IO ()
testSourceRejectsInterleavedUseConstrainedByLaterRecursiveMember =
  assertSourceSingleErrorContains
    "left = if True \\(x) -> x else right.\nbad = left True.\nright = \\(x) -> left (x + 1)."
    "cannot apply function of type Int -> Int to argument of type Bool"

testSourceTypesRecursiveGuardsAgainstPriorRebinding :: IO ()
testSourceTypesRecursiveGuardsAgainstPriorRebinding =
  assertSourceOk "f = \\(x) -> x.\nf = case 0 { | 0 if f True -> \\(y) -> y | _ -> \\(y) -> y }.\nvalue = f 1.\nvalue."

testSourceDefersPartialRecursivePreviewsPastInterveningDependencies :: IO ()
testSourceDefersPartialRecursivePreviewsPastInterveningDependencies =
  assertSourceErrorContains
    "left = if True \\(x) -> x else right.\nearly = left True.\nhelper = \\(x) -> x + 1.\nright = \\(x) -> left (helper x).\nearly."
    "cannot apply function"

testSourcePreviewsThroughInterveningRecursiveGroupMembers :: IO ()
testSourcePreviewsThroughInterveningRecursiveGroupMembers =
  assertSourceOk "left = if True \\(x) -> x else right.\nearly = left True.\nmiddle = if True \\(x) -> x else left.\nright = if False middle else left.\nlate = left 1.\nlate."

testSourceRejectsUnsupportedVariableConstrainedSignatureContract :: IO ()
testSourceRejectsUnsupportedVariableConstrainedSignatureContract = do
  result <- compileSource defaultWarningSettings "f :: @{Eq(a)}: b -> b.\nf = \\(x) -> x."
  assertSingleDiagnosticCode
    "source unsupported variable constrained signature code"
    "E2009"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source unsupported variable constrained signature contract"
    "type-variable constrained signatures require every constrained variable to appear in the signature body"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source unsupported variable constrained signature payload"
    "@{Eq(a)}: b -> b"
    (compileErrors result)

testSourceRejectsUnusedVariableConstraintWithBidirectionalContract :: IO ()
testSourceRejectsUnusedVariableConstraintWithBidirectionalContract = do
  result <- compileSource defaultWarningSettings "f :: @{Eq(a)}: Int -> Int.\nf = \\(x) -> x."
  assertSingleDiagnosticCode
    "source unused variable constraint code"
    "E2009"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source unused variable constraint contract"
    "type-variable constrained signatures require every constrained variable to appear in the signature body"
    (compileErrors result)

testSourceRejectsVariableConstrainedTypeApplicationWithoutShiftingState :: IO ()
testSourceRejectsVariableConstrainedTypeApplicationWithoutShiftingState = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing "bad :: @{Eq(f), Ord(a)}: f(a) -> a.\nbad = \\(x) -> x.\nuse = [] 1."
  assertContains
    "later diagnostic keeps deterministic type variable id"
    "cannot apply function of type [t3] to argument of type Int"
    (Text.unlines (map renderDiagnostic (compileErrors result)))

testSourceKeepsGenericConstructorAliasesMonomorphic :: IO ()
testSourceKeepsGenericConstructorAliasesMonomorphic = do
  result <- compileSource defaultWarningSettings "data Box a = Box a. make = Box. first = make 1. second = make True."
  assertSingleDiagnosticCode
    "generic constructor alias monomorphic code"
    "E2006"
    (compileErrors result)
  assertSingleDiagnosticContains
    "generic constructor alias monomorphic text"
    "cannot apply function of type Int -> Box"
    (compileErrors result)

testSourceRejectsConstrainedSignatureSurface :: IO ()
testSourceRejectsConstrainedSignatureSurface = do
  result <- compileSource defaultWarningSettings "f :: @{Eq(a), Ord(b)}: a -> c.\nf = \\(x) -> x."
  assertSingleDiagnosticCode
    "source constrained signature code"
    "E2009"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source constrained signature payload"
    "@{Eq(a), Ord(b)}: a -> c"
    (compileErrors result)

testSourceReportsSignedRecursiveRhsTypeError :: IO ()
testSourceReportsSignedRecursiveRhsTypeError =
  assertSourceSingleErrorContains "x :: Bool.\nx = x + 1." "E2003"

testSignatureMismatchKeepsDeclaredTypeDownstream :: IO ()
testSignatureMismatchKeepsDeclaredTypeDownstream =
  assertSourceSingleErrorContains "x :: Int.\nx = True.\ny = x + 1." "E2005"

testMismatchedPendingSignatureDoesNotMonomorphizeFollowingBinding :: IO ()
testMismatchedPendingSignatureDoesNotMonomorphizeFollowingBinding =
  assertSourceSingleErrorContains "x :: Int.\nid = \\(value) -> value.\nintValue = id 1.\nboolValue = id True." "E1003"
