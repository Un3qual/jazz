{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.BindingSignature.ConstraintsTests
  ( constraintTests
  ) where

import JazzNext.Compiler.AST
  ( ClassMethodSignature (..),
    Expr (..),
    Literal (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
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
    assertEqual,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    assertSingleDiagnosticPrimarySpan
  )
import JazzNext.Compiler.Semantics.BindingSignature.Shared

constraintTests :: [NamedTest]
constraintTests =
  [ ("source pipeline accepts inert class and impl declarations", testSourceAcceptsCapabilityDeclarations)
    , ("source pipeline accepts class method signature metadata", testSourceAcceptsClassMethodSignatureMetadata)
    , ("source pipeline rejects method-local class signature variables", testSourceRejectsMethodLocalClassSignatureVariables)
    , ("source pipeline rejects constrained class method signatures", testSourceRejectsConstrainedClassMethodSignatures)
    , ("source pipeline rejects unknown named class method signatures", testSourceRejectsUnknownNamedClassMethodSignatures)
    , ("source pipeline rejects duplicate class method signatures", testSourceRejectsDuplicateClassMethodSignatures)
    , ("analyzer rejects duplicate class method metadata", testAnalyzerRejectsDuplicateClassMethodMetadata)
    , ("source pipeline analyzes impl method binding metadata", testSourceAnalyzesImplMethodBindingMetadata)
    , ("source pipeline rejects variable-target impl method bindings", testSourceRejectsVariableTargetImplMethodBindings)
    , ("source pipeline rejects variable-target empty impl declarations", testSourceRejectsVariableTargetEmptyImplDeclarations)
    , ("source pipeline rejects unknown named impl targets", testSourceRejectsUnknownNamedImplTargets)
    , ("source pipeline rejects wrong-arity named impl targets", testSourceRejectsWrongArityNamedImplTargets)
    , ("source pipeline instantiates unconstrained variables beside numeric constraints per use", testSourceInstantiatesUnconstrainedNumericBindingVariablesPerUse)
    , ("source pipeline instantiates unconstrained variables beside equality constraints per use", testSourceInstantiatesUnconstrainedEqualityBindingVariablesPerUse)
    , ("source pipeline infers equality class constraints for ordinary binding schemes", testSourceInfersEqualityClassConstraintsForOrdinaryBindingSchemes)
    , ("source pipeline rejects missing inferred equality facts at use sites", testSourceRejectsMissingInferredEqualityFactAtUseSite)
    , ("source pipeline rejects missing inferred equality facts through operator values", testSourceRejectsMissingInferredEqualityFactThroughOperatorValue)
    , ("source pipeline rejects missing inferred equality facts through sections", testSourceRejectsMissingInferredEqualityFactThroughSection)
    , ("source pipeline accepts primitive equality helpers without visible Eq", testSourceAcceptsPrimitiveEqualityHelperWithoutVisibleEq)
    , ("source pipeline rejects ambiguous inferred equality binding use", testSourceRejectsAmbiguousInferredEqualityBindingUse)
    , ("source pipeline infers qualified method class constraints for ordinary binding schemes", testSourceInfersQualifiedMethodClassConstraintsForOrdinaryBindingSchemes)
    , ("source pipeline resolves inferred method facts through aliases", testSourceResolvesInferredMethodFactsThroughAliases)
    , ("source pipeline rejects result-only qualified method inference", testSourceRejectsResultOnlyQualifiedMethodInference)
    , ("source pipeline rejects unpreserved higher-order qualified method inference", testSourceRejectsUnpreservedHigherOrderQualifiedMethodInference)
    , ("source pipeline preserves inferred method constraints on signed bindings", testSourcePreservesInferredMethodConstraintsOnSignedBindings)
    , ("source pipeline rejects undeclared equality constraints on signed bindings", testSourceRejectsUndeclaredEqualityConstraintsOnSignedBindings)
    , ("source pipeline resolves concrete inferred method obligations before dropping them", testSourceResolvesConcreteInferredMethodObligationsBeforeDroppingThem)
    , ("source pipeline resolves literal-range inferred method obligations before dropping them", testSourceResolvesLiteralRangeInferredMethodObligationsBeforeDroppingThem)
    , ("source pipeline rejects ambiguous dropped literal-range method obligations", testSourceRejectsAmbiguousDroppedLiteralRangeMethodObligations)
    , ("source pipeline preserves literal-range deferred method constraints", testSourcePreservesLiteralRangeDeferredMethodConstraints)
    , ("source pipeline rejects ambiguous literal-range deferred method constraints", testSourceRejectsAmbiguousLiteralRangeDeferredMethodConstraints)
    , ("source pipeline keeps nested helper inferred method obligations scoped", testSourceKeepsNestedHelperInferredMethodObligationsScoped)
    , ("source pipeline preserves outer-scope local inferred method obligations", testSourcePreservesOuterScopeLocalInferredMethodObligations)
    , ("source pipeline resolves concrete inferred equality obligations before dropping them", testSourceResolvesConcreteInferredEqualityObligationsBeforeDroppingThem)
    , ("source pipeline checks inferred method obligations on expression statements", testSourceChecksInferredMethodObligationsOnExpressionStatements)
    , ("source pipeline checks inferred equality obligations on expression statements", testSourceChecksInferredEqualityObligationsOnExpressionStatements)
    , ("source pipeline rejects ambiguous inferred equality obligations on expression statements", testSourceRejectsAmbiguousInferredEqualityObligationsOnExpressionStatements)
    , ("source pipeline checks inferred method obligations on monomorphic signed bindings", testSourceChecksInferredMethodObligationsOnMonomorphicSignedBindings)
    , ("source pipeline rejects exact matches from non-target qualified method arguments", testSourceRejectsNonTargetQualifiedMethodExactMatch)
    , ("source pipeline rejects callable equality before inferred class obligations", testSourceRejectsCallableEqualityBeforeInferredClassObligations)
    , ("source pipeline rejects duplicate impl method bindings", testSourceRejectsDuplicateImplMethodBindings)
    , ("source pipeline rejects non-binding impl body items", testSourceRejectsNonBindingImplBodyItem)
    , ("source pipeline accepts single-target qualified method dispatch", testSourceAcceptsSingleTargetQualifiedMethodDispatch)
    , ("source pipeline selects qualified method body by argument types", testSourceSelectsQualifiedMethodBodyByArgumentTypes)
    , ("source pipeline rejects nested empty-list exact qualified method selection", testSourceRejectsNestedEmptyListExactQualifiedMethodSelection)
    , ("source pipeline rejects constructor-wrapped nested empty-list exact qualified method selection", testSourceRejectsConstructorWrappedNestedEmptyListExactQualifiedMethodSelection)
    , ("source pipeline rejects opaque nested empty-list exact qualified method selection", testSourceRejectsOpaqueNestedEmptyListExactQualifiedMethodSelection)
    , ("source pipeline rejects opaque list application exact qualified method selection", testSourceRejectsOpaqueListApplicationExactQualifiedMethodSelection)
    , ("source pipeline rejects block-produced nested empty-list exact qualified method selection", testSourceRejectsBlockProducedNestedEmptyListExactQualifiedMethodSelection)
    , ("source pipeline rejects control-flow nested empty-list exact qualified method selection", testSourceRejectsControlFlowNestedEmptyListExactQualifiedMethodSelection)
    , ("source pipeline selects qualified Float method body by argument types", testSourceSelectsQualifiedFloatMethodBodyByArgumentTypes)
    , ("source pipeline selects qualified Float16 method body by argument types", testSourceSelectsQualifiedFloat16MethodBodyByArgumentTypes)
    , ("source pipeline selects qualified Float32 method body by argument types", testSourceSelectsQualifiedFloat32MethodBodyByArgumentTypes)
    , ("source pipeline selects qualified Float64 method body by argument types", testSourceSelectsQualifiedFloat64MethodBodyByArgumentTypes)
    , ("source pipeline selects qualified method body through prefix dollar", testSourceSelectsQualifiedMethodBodyThroughPrefixDollar)
    , ("source pipeline accepts same-impl qualified method body references", testSourceAcceptsSameImplQualifiedMethodBodyReferences)
    , ("source pipeline uses impl signatures while checking method bodies", testSourceUsesImplSignaturesWhileCheckingMethodBodies)
    , ("source pipeline uses impl signatures to contextualize method body lambdas", testSourceUsesImplSignaturesToContextualizeMethodBodyLambdas)
    , ("source pipeline accepts higher-order qualified method signature", testSourceAcceptsHigherOrderQualifiedMethodSignature)
    , ("source pipeline prefers visible binding over qualified method spine", testSourcePrefersVisibleBindingOverQualifiedMethodSpine)
    , ("source pipeline applies substituted qualified method signature", testSourceRejectsQualifiedMethodSignatureMismatch)
    , ("source pipeline rejects qualified method dispatch with no typed candidate", testSourceRejectsQualifiedMethodDispatchWithNoTypedCandidate)
    , ("source pipeline rejects qualified impl method body mismatch", testSourceRejectsQualifiedImplMethodBodyMismatch)
    , ("source pipeline rejects impl method before class method metadata", testSourceRejectsImplMethodBeforeClassMethodMetadata)
    , ("source pipeline rejects qualified dispatch without class method metadata", testSourceRejectsQualifiedMethodMissingClassMethod)
    , ("source pipeline rejects qualified dispatch without impl method body", testSourceRejectsQualifiedMethodMissingImplBody)
    , ("source pipeline rejects deferred qualified method requirement without impl method body", testSourceRejectsDeferredQualifiedMethodRequirementMissingImplBody)
    , ("source pipeline rejects ambiguous qualified method bodies", testSourceRejectsAmbiguousQualifiedMethodBodies)
    , ("source pipeline rejects duplicate class declarations", testSourceRejectsDuplicateClassDeclarations)
    , ("source pipeline rejects duplicate concrete impl declarations", testSourceRejectsDuplicateConcreteImplDeclarations)
    , ("source pipeline rejects duplicate ADT impl declarations", testSourceRejectsDuplicateAdtImplDeclarations)
    , ("compiler exposes imported qualified method bodies", testCompilerExposesImportedQualifiedMethodBodies)
    , ("source pipeline accepts simple function signature", testSourceAcceptsSimpleFunctionSignature)
    , ("source pipeline rejects concrete constrained signature without impl fact", testSourceRejectsConcreteConstrainedSignatureWithoutImplFact)
    , ("source pipeline rejects unknown constrained signature constraint", testSourceRejectsUnknownConstrainedSignatureConstraint)
    , ("source pipeline rejects wrong-arity constrained signature constraint", testSourceRejectsWrongArityConstrainedSignatureConstraint)
    , ("source pipeline reports duplicate constrained signature constraints", testSourceRejectsDuplicateConstrainedSignatureConstraints)
    , ("source pipeline instantiates equality constrained signatures per use", testSourceInstantiatesEqualityConstrainedSignaturePerUse)
    , ("source pipeline accepts unconstrained variables beside explicit constraints", testSourceAcceptsUnconstrainedVariablesBesideExplicitConstraints)
    , ("source pipeline preserves primitive constraints on variable constrained signatures", testSourcePreservesPrimitiveConstraintsOnVariableConstrainedSignatures)
    , ("source pipeline rejects undeclared primitive constraints on signed bindings", testSourceRejectsUndeclaredPrimitiveConstraintsOnSignedBindings)
    , ("source pipeline rejects undeclared class constraints on signed bindings", testSourceRejectsUndeclaredClassConstraintsOnSignedBindings)
    , ("source pipeline preserves explicit constraints when primitive RHS has no quantified variables", testSourcePreservesExplicitConstraintsWhenPrimitiveRhsHasNoQuantifiedVariables)
    , ("source pipeline preserves explicit Eq impl checks for structural constraints", testSourcePreservesExplicitEqImplChecksForStructuralConstraints)
    , ("source pipeline resolves deferred constraints in impl method bodies", testSourceResolvesDeferredConstraintsInImplMethodBodies)
    , ("source pipeline discards failed application argument constraints", testSourceDiscardsFailedApplicationArgumentConstraints)
    , ("source pipeline discards failed application function constraints", testSourceDiscardsFailedApplicationFunctionConstraints)
    , ("source pipeline rejects unused variable constraint with bidirectional contract", testSourceRejectsUnusedVariableConstraintWithBidirectionalContract)
  ]

testSourceAcceptsCapabilityDeclarations :: IO ()
testSourceAcceptsCapabilityDeclarations =
  assertSourceOkWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Int) { }.
  x :: Int.
  x = 1.
  x.
  """

testSourceAcceptsClassMethodSignatureMetadata :: IO ()
testSourceAcceptsClassMethodSignatureMetadata =
  assertSourceOkWithoutPrelude """
  class Eq(a) {
  equals :: a -> a -> Bool.
  notEquals :: a -> a -> Bool.
  }.
  impl Eq(Int) { }.
  x :: Int.
  x = 1.
  x.
  """

testSourceRejectsMethodLocalClassSignatureVariables :: IO ()
testSourceRejectsMethodLocalClassSignatureVariables =
  assertSourceSingleErrorContainsWithoutPrelude
    "class C(a) { f :: b -> b. }."
    "method-local type variable 'b'"

testSourceRejectsConstrainedClassMethodSignatures :: IO ()
testSourceRejectsConstrainedClassMethodSignatures =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Need(a) { }.
    class C(a) { m :: @{Need(a)}: a -> Bool. }.
    0.
    """
    "invalid or unsupported class method signature for 'C::m'"

testSourceRejectsUnknownNamedClassMethodSignatures :: IO ()
testSourceRejectsUnknownNamedClassMethodSignatures =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class C(a) { f :: Unknown -> a. }.
    x = 1.
    """
    "unknown named type 'Unknown'"

testSourceRejectsDuplicateClassMethodSignatures :: IO ()
testSourceRejectsDuplicateClassMethodSignatures =
  assertSourceSingleErrorContainsWithoutPrelude """
  class Eq(a) { equals :: Int. equals :: Bool. }.
  x = 1.
  """ "duplicate method signature 'equals'"

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
  assertSourceOkWithoutPrelude """
  class Eq(a) {
  equals :: a -> a -> Bool.
  }.
  impl Eq(Int) {
  equals = \\(left, right) -> left == right.
  }.
  x :: Int.
  x = 1.
  x.
  """
  assertSourceSingleErrorContainsWithoutPrelude """
  class Eq(a) {
  equals :: a -> a -> Bool.
  }.
  impl Eq(Int) {
  equals = missingImplRuntime.
  }.
  x :: Int.
  x = 1.
  x.
  """ "unbound variable 'missingImplRuntime'"

testSourceRejectsVariableTargetImplMethodBindings :: IO ()
testSourceRejectsVariableTargetImplMethodBindings =
  assertSourceSingleErrorContainsWithoutPrelude """
  class Eq(a) { }.
  impl Eq(a) { equals = 1. }.
  x = 1.
  """ "concrete impl target"

testSourceRejectsVariableTargetEmptyImplDeclarations :: IO ()
testSourceRejectsVariableTargetEmptyImplDeclarations =
  assertSourceSingleErrorContainsWithoutPrelude """
  class Eq(a) { }.
  impl Eq(a) { }.
  x = 1.
  """ "concrete impl target"

testSourceRejectsUnknownNamedImplTargets :: IO ()
testSourceRejectsUnknownNamedImplTargets =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) { }.
    impl Eq(Unknown(Char)) { }.
    x = 1.
    """
    "unknown named type 'Unknown'"

testSourceRejectsWrongArityNamedImplTargets :: IO ()
testSourceRejectsWrongArityNamedImplTargets =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    data Box a = Box a.
    class Eq(a) { }.
    impl Eq(Box(Int, Bool)) { }.
    x = 1.
    """
    "type 'Box' expects 1 argument(s), found 2"

testSourceInstantiatesUnconstrainedNumericBindingVariablesPerUse :: IO ()
testSourceInstantiatesUnconstrainedNumericBindingVariablesPerUse =
  assertSourceOk """
  f = \\(x, y) -> (x + x, y).
  a = f 1 True.
  b = f 2 3.
  """

testSourceInstantiatesUnconstrainedEqualityBindingVariablesPerUse :: IO ()
testSourceInstantiatesUnconstrainedEqualityBindingVariablesPerUse =
  assertSourceOk """
  f = \\(x, y) -> (x == x, y).
  a = f 1 True.
  b = f 2 3.
  """

testSourceInfersEqualityClassConstraintsForOrdinaryBindingSchemes :: IO ()
testSourceInfersEqualityClassConstraintsForOrdinaryBindingSchemes =
  assertSourceOkWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Int) { }.
  impl Eq(Bool) { }.
  same = \\(left, right) -> left == right.
  intResult = same 1 1.
  boolResult = same True False.
  """

testSourceRejectsMissingInferredEqualityFactAtUseSite :: IO ()
testSourceRejectsMissingInferredEqualityFactAtUseSite =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) { }.
    impl Eq(Int) { }.
    same = \\(left, right) -> left == right.
    intResult = same 1 1.
    bad = same True False.
    """
    "missing impl fact 'Eq(Bool)'"

testSourceRejectsMissingInferredEqualityFactThroughOperatorValue :: IO ()
testSourceRejectsMissingInferredEqualityFactThroughOperatorValue =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) { }.
    impl Eq(Int) { }.
    same = (==).
    intResult = same 1 1.
    bad = same True False.
    """
    "missing impl fact 'Eq(Bool)'"

testSourceRejectsMissingInferredEqualityFactThroughSection :: IO ()
testSourceRejectsMissingInferredEqualityFactThroughSection =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) { }.
    impl Eq(Int) { }.
    same = \\(right) -> (== right).
    intResult = same 1 1.
    bad = same True False.
    """
    "missing impl fact 'Eq(Bool)'"

testSourceAcceptsPrimitiveEqualityHelperWithoutVisibleEq :: IO ()
testSourceAcceptsPrimitiveEqualityHelperWithoutVisibleEq =
  assertSourceOkWithoutPrelude """
  same = \\(x) -> x == x.
  ok = same 1.
  """

testSourceRejectsAmbiguousInferredEqualityBindingUse :: IO ()
testSourceRejectsAmbiguousInferredEqualityBindingUse =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class Eq(a) { }.
    ambiguous = \\(x) -> x == x.
    ambiguous.
    """
    )
    "ambiguous/defaulting inferred constraint 'Eq"

testSourceInfersQualifiedMethodClassConstraintsForOrdinaryBindingSchemes :: IO ()
testSourceInfersQualifiedMethodClassConstraintsForOrdinaryBindingSchemes =
  assertSourceOkWithoutPrelude
    ( """
    class Eq(a) {
    equals :: a -> a -> Bool.
    }.
    impl Eq(Int) {
    equals = \\(left, right) -> left == right.
    }.
    impl Eq(Bool) {
    equals = \\(left, right) -> left == right.
    }.
    same = \\(left, right) -> Eq::equals left right.
    intResult = same 1 1.
    boolResult = same True False.
    """
    )

testSourceResolvesInferredMethodFactsThroughAliases :: IO ()
testSourceResolvesInferredMethodFactsThroughAliases =
  assertSourceOkWithoutPrelude
    ( """
    class Eq(a) {
    equals :: a -> a -> Bool.
    }.
    impl Eq(Float) {
    equals = \\(left, right) -> left == right.
    }.
    value :: Float64.
    value = 1.5.
    same = \\(x) -> Eq::equals x x.
    result = same value.
    """
    )

testSourceRejectsResultOnlyQualifiedMethodInference :: IO ()
testSourceRejectsResultOnlyQualifiedMethodInference =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class Make(a) {
    make :: Int -> a.
    }.
    impl Make(Int) {
    make = \\(value) -> value.
    }.
    impl Make(Bool) {
    make = \\(value) -> True.
    }.
    x :: Int.
    x = Make::make 0.
    """
    )
    "ambiguous qualified method body"

testSourceRejectsUnpreservedHigherOrderQualifiedMethodInference :: IO ()
testSourceRejectsUnpreservedHigherOrderQualifiedMethodInference =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class C(a) {
    m :: (a -> a) -> Bool.
    }.
    impl C(Int) {
    m = \\(f) -> True.
    }.
    impl C(Bool) {
    m = \\(f) -> False.
    }.
    f = \\(x) -> x.
    result = C::m f.
    """
    )
    "ambiguous qualified method body"

testSourcePreservesInferredMethodConstraintsOnSignedBindings :: IO ()
testSourcePreservesInferredMethodConstraintsOnSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int) { }.
    f :: @{C(a)}: a -> Bool.
    f = \\(x) -> C::m x.
    result = f 1.
    """
    )
    "missing impl method body 'C::m'"

testSourceRejectsUndeclaredEqualityConstraintsOnSignedBindings :: IO ()
testSourceRejectsUndeclaredEqualityConstraintsOnSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class Eq(a) { }.
    class C(a) { }.
    same :: @{C(a)}: a -> a -> Bool.
    same = \\(x, y) -> x == y.
    """
    )
    "does not declare required constraint 'Eq"

testSourceResolvesConcreteInferredMethodObligationsBeforeDroppingThem :: IO ()
testSourceResolvesConcreteInferredMethodObligationsBeforeDroppingThem =
  assertSourceOkWithoutPrelude
    ( """
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int) {
    m = \\(x) -> True.
    }.
    result = (\\(x) -> C::m x) 1.
    """
    )

testSourceResolvesLiteralRangeInferredMethodObligationsBeforeDroppingThem :: IO ()
testSourceResolvesLiteralRangeInferredMethodObligationsBeforeDroppingThem =
  assertSourceOkWithoutPrelude
    ( """
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int8) {
    m = \\(x) -> True.
    }.
    result = (\\(x) -> C::m x) 1.
    """
    )

testSourceRejectsAmbiguousDroppedLiteralRangeMethodObligations :: IO ()
testSourceRejectsAmbiguousDroppedLiteralRangeMethodObligations =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int8) {
    m = \\(x) -> True.
    }.
    impl C(Int16) {
    m = \\(x) -> False.
    }.
    result = (\\(x) -> C::m x) 1.
    """
    )
    "ambiguous qualified method body 'C::m'"

testSourcePreservesLiteralRangeDeferredMethodConstraints :: IO ()
testSourcePreservesLiteralRangeDeferredMethodConstraints =
  assertSourceOkWithoutPrelude
    ( """
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int8) {
    m = \\(x) -> True.
    }.
    f = \\(x) -> C::m x.
    result = f 1.
    """
    )

testSourceRejectsAmbiguousLiteralRangeDeferredMethodConstraints :: IO ()
testSourceRejectsAmbiguousLiteralRangeDeferredMethodConstraints =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int8) {
    m = \\(x) -> True.
    }.
    impl C(Int16) {
    m = \\(x) -> False.
    }.
    f = \\(x) -> C::m x.
    result = f 1.
    """
    )
    "ambiguous qualified method body 'C::m'"

testSourceKeepsNestedHelperInferredMethodObligationsScoped :: IO ()
testSourceKeepsNestedHelperInferredMethodObligationsScoped =
  assertSourceOkWithoutPrelude
    ( """
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int) {
    m = \\(x) -> True.
    }.
    x = { local = \\(y) -> C::m y. 1. }.
    x.
    """
    )

testSourcePreservesOuterScopeLocalInferredMethodObligations :: IO ()
testSourcePreservesOuterScopeLocalInferredMethodObligations =
  assertSourceOkWithoutPrelude
    ( """
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int) {
    m = \\(x) -> True.
    }.
    outer = \\(x) -> { local = C::m x. 1. }.
    result = outer 1.
    """
    )

testSourceResolvesConcreteInferredEqualityObligationsBeforeDroppingThem :: IO ()
testSourceResolvesConcreteInferredEqualityObligationsBeforeDroppingThem =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class Eq(a) { }.
    impl Eq(Int) { }.
    result = (\\(x) -> x == x) True.
    """
    )
    "missing impl fact 'Eq(Bool)'"

testSourceChecksInferredMethodObligationsOnExpressionStatements :: IO ()
testSourceChecksInferredMethodObligationsOnExpressionStatements =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int) { }.
    (\\(x) -> C::m x) 1.
    """
    )
    "missing impl method body 'C::m'"

testSourceChecksInferredEqualityObligationsOnExpressionStatements :: IO ()
testSourceChecksInferredEqualityObligationsOnExpressionStatements =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class Eq(a) { }.
    impl Eq(Int) { }.
    (\\(x) -> x == x) True.
    """
    )
    "missing impl fact 'Eq(Bool)'"

testSourceRejectsAmbiguousInferredEqualityObligationsOnExpressionStatements :: IO ()
testSourceRejectsAmbiguousInferredEqualityObligationsOnExpressionStatements =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class Eq(a) { }.
    \\(x) -> x == x.
    """
    )
    "ambiguous/defaulting inferred constraint 'Eq"

testSourceChecksInferredMethodObligationsOnMonomorphicSignedBindings :: IO ()
testSourceChecksInferredMethodObligationsOnMonomorphicSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int) { }.
    result :: Bool.
    result = (\\(x) -> C::m x) 1.
    """
    )
    "missing impl method body 'C::m'"

testSourceRejectsNonTargetQualifiedMethodExactMatch :: IO ()
testSourceRejectsNonTargetQualifiedMethodExactMatch =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class Flag(a) {
    flag :: Int -> Bool.
    }.
    impl Flag(Int) {
    flag = \\(value) -> True.
    }.
    impl Flag(Bool) {
    flag = \\(value) -> False.
    }.
    one :: Int.
    one = 1.
    result = Flag::flag one.
    """
    )
    "ambiguous qualified method body"

testSourceRejectsCallableEqualityBeforeInferredClassObligations :: IO ()
testSourceRejectsCallableEqualityBeforeInferredClassObligations =
  assertSourceSingleErrorContains
    """
    f = \\(x) -> x.
    bad = f == f.
    """
    "callable values are not equality-supported"

testSourceRejectsDuplicateImplMethodBindings :: IO ()
testSourceRejectsDuplicateImplMethodBindings =
  assertSourceSingleErrorContainsWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Int) { equals = 1. equals = 2. }.
  x = 1.
  """ "duplicate method binding 'equals'"

testSourceRejectsNonBindingImplBodyItem :: IO ()
testSourceRejectsNonBindingImplBodyItem =
  assertSourceSingleErrorContainsWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Int) { equals :: Int. }.
  x = 1.
  """ "ordinary method binding"

testSourceAcceptsSingleTargetQualifiedMethodDispatch :: IO ()
testSourceAcceptsSingleTargetQualifiedMethodDispatch =
  assertSourceOkWithoutPrelude
    ( qualifiedEqSource
        <> """
        result :: Bool.
        result = Eq::equals 1 1.
        result.
        """
    )

testSourceSelectsQualifiedMethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedMethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( qualifiedEqSource
        <> """
        impl Eq(Bool) {
        equals = \\(left, right) -> left == right.
        }.
        result :: Bool.
        result = Eq::equals True False.
        result.
        """
    )

testSourceRejectsNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceRejectsNestedEmptyListExactQualifiedMethodSelection =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class RuntimeFlag(a) {
    flag :: a -> Bool.
    }.
    impl RuntimeFlag([[Int]]) {
    flag = \\(values) -> True.
    }.
    impl RuntimeFlag([[Int64]]) {
    flag = \\(values) -> False.
    }.
    (RuntimeFlag::flag) [[1], []].
    """
    )
    "ambiguous qualified method body 'RuntimeFlag::flag'"

testSourceRejectsConstructorWrappedNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceRejectsConstructorWrappedNestedEmptyListExactQualifiedMethodSelection =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    data Box a = Box a.
    class RuntimeFlag(a) {
    flag :: a -> Bool.
    }.
    impl RuntimeFlag(Box([[Int]])) {
    flag = \\(box) -> True.
    }.
    impl RuntimeFlag(Box([[Int64]])) {
    flag = \\(box) -> False.
    }.
    (RuntimeFlag::flag) (Box [[1], []]).
    """
    )
    "ambiguous qualified method body 'RuntimeFlag::flag'"

testSourceRejectsOpaqueNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceRejectsOpaqueNestedEmptyListExactQualifiedMethodSelection =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    data Box a = Box a.
    class RuntimeFlag(a) {
    flag :: a -> Bool.
    }.
    impl RuntimeFlag(Box([[Int]])) {
    flag = \\(box) -> True.
    }.
    impl RuntimeFlag(Box([[Int64]])) {
    flag = \\(box) -> False.
    }.
    make = \\(values) -> Box values.
    (RuntimeFlag::flag) (make [[1], []]).
    """
    )
    "ambiguous qualified method body 'RuntimeFlag::flag'"

testSourceRejectsOpaqueListApplicationExactQualifiedMethodSelection :: IO ()
testSourceRejectsOpaqueListApplicationExactQualifiedMethodSelection =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class RuntimeFlag(a) {
    flag :: a -> Bool.
    }.
    impl RuntimeFlag([[Int]]) {
    flag = \\(values) -> True.
    }.
    impl RuntimeFlag([[Int64]]) {
    flag = \\(values) -> False.
    }.
    make = \\(values) -> values.
    (RuntimeFlag::flag) (make [[1], []]).
    """
    )
    "ambiguous qualified method body 'RuntimeFlag::flag'"

testSourceRejectsBlockProducedNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceRejectsBlockProducedNestedEmptyListExactQualifiedMethodSelection =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class RuntimeFlag(a) {
    flag :: a -> Bool.
    }.
    impl RuntimeFlag([[Int]]) {
    flag = \\(values) -> True.
    }.
    impl RuntimeFlag([[Int64]]) {
    flag = \\(values) -> False.
    }.
    (RuntimeFlag::flag) { values = [[1], []].
    values. }.
    """
    )
    "ambiguous qualified method body 'RuntimeFlag::flag'"

testSourceRejectsControlFlowNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceRejectsControlFlowNestedEmptyListExactQualifiedMethodSelection =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class RuntimeFlag(a) {
    flag :: a -> Bool.
    }.
    impl RuntimeFlag([[Int]]) {
    flag = \\(values) -> True.
    }.
    impl RuntimeFlag([[Int64]]) {
    flag = \\(values) -> False.
    }.
    (RuntimeFlag::flag) (if True then [[1], []] else [[1], []]).
    """
    )
    "ambiguous qualified method body 'RuntimeFlag::flag'"

testSourceSelectsQualifiedFloatMethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloatMethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( """
    class Eq(a) {
    equals :: a -> a -> Bool.
    }.
    impl Eq(Float) {
    equals = \\(left, right) -> left == right.
    }.
    left :: Float.
    left = 1.5.
    right :: Float.
    right = 2.25.
    result :: Bool.
    result = Eq::equals left right.
    result.
    """
    )

testSourceSelectsQualifiedFloat16MethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloat16MethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( """
    class Eq(a) {
    equals :: a -> a -> Bool.
    }.
    impl Eq(Float16) {
    equals = \\(left, right) -> left == right.
    }.
    left :: Float16.
    left = 1.5.
    right :: Float16.
    right = 2.25.
    result :: Bool.
    result = Eq::equals left right.
    result.
    """
    )

testSourceSelectsQualifiedFloat32MethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloat32MethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( """
    class Eq(a) {
    equals :: a -> a -> Bool.
    }.
    impl Eq(Float32) {
    equals = \\(left, right) -> left == right.
    }.
    left :: Float32.
    left = 1.5.
    right :: Float32.
    right = 2.25.
    result :: Bool.
    result = Eq::equals left right.
    result.
    """
    )

testSourceSelectsQualifiedFloat64MethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloat64MethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( """
    class Eq(a) {
    equals :: a -> a -> Bool.
    }.
    impl Eq(Float64) {
    equals = \\(left, right) -> left == right.
    }.
    left :: Float64.
    left = 1.5.
    right :: Float64.
    right = 2.25.
    result :: Bool.
    result = Eq::equals left right.
    result.
    """
    )

testSourceSelectsQualifiedMethodBodyThroughPrefixDollar :: IO ()
testSourceSelectsQualifiedMethodBodyThroughPrefixDollar =
  assertSourceOkWithoutPrelude
    ( """
    class Choice(a) {
    pick :: a -> Bool.
    }.
    impl Choice(Int) {
    pick = \\(value) -> True.
    }.
    impl Choice(Bool) {
    pick = \\(value) -> False.
    }.
    result :: Bool.
    result = ($) Choice::pick True.
    result.
    """
    )

testSourceAcceptsSameImplQualifiedMethodBodyReferences :: IO ()
testSourceAcceptsSameImplQualifiedMethodBodyReferences =
  assertSourceOkWithoutPrelude
    ( """
    class Eq(a) {
    equals :: a -> a -> Bool.
    notEquals :: a -> a -> Bool.
    }.
    impl Eq(Int) {
    equals = \\(left, right) -> left == right.
    notEquals = \\(left, right) -> Eq::equals left right != True.
    }.
    result :: Bool.
    result = Eq::notEquals 1 2.
    result.
    """
    )

testSourceUsesImplSignaturesWhileCheckingMethodBodies :: IO ()
testSourceUsesImplSignaturesWhileCheckingMethodBodies =
  assertSourceOkWithoutPrelude
    ( """
    class Check(a) {
    check :: a -> Bool.
    notCheck :: a -> Bool.
    }.
    impl Check(Int) {
    check = \\(value) -> True.
    notCheck = \\(value) -> Check::check value != True.
    }.
    impl Check(Bool) {
    check = \\(value) -> False.
    notCheck = \\(value) -> Check::check value != True.
    }.
    result :: Bool.
    result = Check::notCheck 1.
    result.
    """
    )

testSourceUsesImplSignaturesToContextualizeMethodBodyLambdas :: IO ()
testSourceUsesImplSignaturesToContextualizeMethodBodyLambdas =
  assertSourceOkWithoutPrelude
    ( """
    class D(a) {
    n :: a -> Bool.
    }.
    impl D(Int) {
    n = \\(value) -> True.
    }.
    impl D(Bool) {
    n = \\(value) -> False.
    }.
    class C(a) {
    m :: a -> Bool.
    }.
    impl C(Int) {
    m = \\(x) -> D::n x.
    }.
    result :: Bool.
    result = C::m 1.
    result.
    """
    )

testSourceAcceptsHigherOrderQualifiedMethodSignature :: IO ()
testSourceAcceptsHigherOrderQualifiedMethodSignature =
  assertSourceOkWithoutPrelude
    ( """
    class Apply(a) {
    apply :: (Int -> Int) -> Int.
    }.
    impl Apply(Int) {
    apply = \\(f) -> f 1.
    }.
    result :: Int.
    result = Apply::apply (+ 1).
    result.
    """
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
    ( qualifiedEqSource
        <> """
        result = Eq::equals 1 True.
        result.
        """
    )
    "cannot apply function of type Int -> Bool to argument of type Bool"

testSourceRejectsQualifiedMethodDispatchWithNoTypedCandidate :: IO ()
testSourceRejectsQualifiedMethodDispatchWithNoTypedCandidate =
  assertSourceSingleErrorContainsWithoutPrelude
    ( qualifiedEqSource
        <> """
        impl Eq(Bool) {
        equals = \\(left, right) -> left == right.
        }.
        result = Eq::equals 1 False.
        result.
        """
    )
    "no matching qualified method body 'Eq::equals' for argument types Int, Bool"

testSourceRejectsQualifiedImplMethodBodyMismatch :: IO ()
testSourceRejectsQualifiedImplMethodBodyMismatch =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) {
    equals :: a -> a -> Bool.
    }.
    impl Eq(Int) {
    equals = 1.
    }.
    result = Eq::equals 1 1.
    result.
    """
    "impl method 'Eq::equals' declared as Int -> Int -> Bool but inferred as Int"

testSourceRejectsImplMethodBeforeClassMethodMetadata :: IO ()
testSourceRejectsImplMethodBeforeClassMethodMetadata = do
  result <-
    compileSourceWithPrelude
      defaultWarningSettings
      Nothing
      """
      impl Eq(Int) {
      equals = 1.
      }.
      class Eq(a) {
      equals :: a -> a -> Bool.
      }.
      result = Eq::equals 1 1.
      result.
      """
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
    """
    class Eq(a) { }.
    impl Eq(Int) {
    equals = \\(left, right) -> left == right.
    }.
    result = Eq::equals 1 1.
    result.
    """
    "class method metadata for 'Eq::equals' must be declared before impl method body"

testSourceRejectsQualifiedMethodMissingImplBody :: IO ()
testSourceRejectsQualifiedMethodMissingImplBody =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) {
    equals :: a -> a -> Bool.
    }.
    impl Eq(Int) { }.
    result = Eq::equals 1 1.
    result.
    """
    "missing impl method body 'Eq::equals'"

testSourceRejectsDeferredQualifiedMethodRequirementMissingImplBody :: IO ()
testSourceRejectsDeferredQualifiedMethodRequirementMissingImplBody =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) {
    equals :: a -> a -> Bool.
    }.
    impl Eq(Int) { }.
    same = \\(x) -> Eq::equals x x.
    result = same 1.
    """
    "missing impl method body 'Eq::equals'"

testSourceRejectsAmbiguousQualifiedMethodBodies :: IO ()
testSourceRejectsAmbiguousQualifiedMethodBodies =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Classify(a) {
    classify :: Int -> Bool.
    }.
    impl Classify(Int) {
    classify = \\(value) -> value == 1.
    }.
    impl Classify(Bool) {
    classify = \\(value) -> value == 2.
    }.
    result = Classify::classify 1.
    result.
    """
    "ambiguous qualified method body 'Classify::classify' for argument types Int"

testSourceRejectsDuplicateClassDeclarations :: IO ()
testSourceRejectsDuplicateClassDeclarations =
  assertSourceSingleErrorContainsWithoutPrelude """
  class Eq(a) { }.
  class Eq(b) { }.
  x = 1.
  """ "E1004"

testSourceRejectsDuplicateConcreteImplDeclarations :: IO ()
testSourceRejectsDuplicateConcreteImplDeclarations = do
  assertSourceSingleErrorContainsWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Int) { }.
  impl Eq(Int) { }.
  x = 1.
  """ "E1005"
  assertSourceSingleErrorContainsWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Float) { }.
  impl Eq(Float) { }.
  x = 1.
  """ "E1005"
  assertSourceSingleErrorContainsWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Float64) { }.
  impl Eq(Float64) { }.
  x = 1.
  """ "E1005"

testSourceRejectsDuplicateAdtImplDeclarations :: IO ()
testSourceRejectsDuplicateAdtImplDeclarations = do
  assertSourceSingleErrorContainsWithoutPrelude """
  data Color = Red.
  class Eq(a) { }.
  impl Eq(Color) { }.
  impl Eq(Color) { }.
  x = 1.
  """ "E1005"
  assertSourceSingleErrorContainsWithoutPrelude """
  data Box a = Box a.
  class Eq(a) { }.
  impl Eq(Box(Int)) { }.
  impl Eq(Box(Int)) { }.
  x = 1.
  """ "E1005"

testCompilerExposesImportedQualifiedMethodBodies :: IO ()
testCompilerExposesImportedQualifiedMethodBodies = do
  result <- compileExpr defaultWarningSettings importedQualifiedMethodFactsProgram
  assertEqual "imported qualified method compile errors" [] (compileErrors result)

testSourceAcceptsSimpleFunctionSignature :: IO ()
testSourceAcceptsSimpleFunctionSignature =
  assertSourceOk """
  f :: Int -> Int.
  f = (+ 1).
  """

testSourceRejectsConcreteConstrainedSignatureWithoutImplFact :: IO ()
testSourceRejectsConcreteConstrainedSignatureWithoutImplFact =
  assertSourceSingleErrorContainsWithoutPrelude """
  class Eq(a) { }.
  x :: @{Eq(Int)}: Int.
  x = 1.
  """ "missing impl fact 'Eq(Int)'"

testSourceRejectsUnknownConstrainedSignatureConstraint :: IO ()
testSourceRejectsUnknownConstrainedSignatureConstraint =
  assertSourceSingleErrorContains """
  x :: @{Unknown(Int)}: Int.
  x = 1.
  """ "E2009"

testSourceRejectsWrongArityConstrainedSignatureConstraint :: IO ()
testSourceRejectsWrongArityConstrainedSignatureConstraint =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) { }.
    impl Eq(Int) { }.
    x :: @{Eq(Int, Bool)}: Int.
    x = 1.
    """
    "constraint 'Eq' expects 1 argument(s), got 2"

testSourceRejectsDuplicateConstrainedSignatureConstraints :: IO ()
testSourceRejectsDuplicateConstrainedSignatureConstraints = do
  result <- compileSource defaultWarningSettings """
  f :: @{Eq(a), Eq(a)}: a -> a.
  f = \\(x) -> x.
  """
  assertSingleDiagnosticCode
    "source duplicate constrained signature code"
    "E2009"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source duplicate constrained signature text"
    "duplicate constraint 'Eq'"
    (compileErrors result)

testSourceInstantiatesEqualityConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesEqualityConstrainedSignaturePerUse =
  assertSourceOkWithoutPrelude """
  class Eq(a) { }.
  impl Eq(Int) { }.
  impl Eq(Bool) { }.
  same :: @{Eq(a)}: a -> a -> Bool.
  same = \\(x, y) -> x == y.
  intValue = same 1 2.
  boolValue = same True False.
  """

testSourceAcceptsUnconstrainedVariablesBesideExplicitConstraints :: IO ()
testSourceAcceptsUnconstrainedVariablesBesideExplicitConstraints =
  assertSourceOk """
  choose :: @{Eq(a)}: a -> b -> a.
  choose = \\(x, y) -> x.
  intBool = choose 1 True.
  intInt = choose 2 3.
  """

testSourcePreservesPrimitiveConstraintsOnVariableConstrainedSignatures :: IO ()
testSourcePreservesPrimitiveConstraintsOnVariableConstrainedSignatures =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Num(a) { }.
    impl Num(Bool) { }.
    addSelf :: @{Num(a)}: a -> a.
    addSelf = \\(x) -> x + x.
    bad = addSelf True.
    """
    "cannot apply function"

testSourceRejectsUndeclaredPrimitiveConstraintsOnSignedBindings :: IO ()
testSourceRejectsUndeclaredPrimitiveConstraintsOnSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    bad :: a -> a.
    bad = \\(x) -> x + 1.
    """
    "does not declare required primitive constraint"

testSourceRejectsUndeclaredClassConstraintsOnSignedBindings :: IO ()
testSourceRejectsUndeclaredClassConstraintsOnSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
    class Show(a) { show :: a -> Bool. }.
    impl Show(Int) { show = \\(x) -> True. }.
    bad :: a -> Bool.
    bad = \\(x) -> Show::show x.
    """
    )
    "does not declare required constraint 'Show"

testSourcePreservesExplicitConstraintsWhenPrimitiveRhsHasNoQuantifiedVariables :: IO ()
testSourcePreservesExplicitConstraintsWhenPrimitiveRhsHasNoQuantifiedVariables =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Num(a) { }.
    addSelf :: @{Num(a)}: a -> a.
    addSelf = \\(x) -> x + x.
    good = addSelf 1.
    """
    "missing impl fact 'Num(Int)'"

testSourcePreservesExplicitEqImplChecksForStructuralConstraints :: IO ()
testSourcePreservesExplicitEqImplChecksForStructuralConstraints =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) { }.
    xs :: @{Eq([Int])}: [Int].
    xs = [1].
    """
    "missing impl fact 'Eq([Int])'"

testSourceResolvesDeferredConstraintsInImplMethodBodies :: IO ()
testSourceResolvesDeferredConstraintsInImplMethodBodies =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Eq(a) { }.
    impl Eq(Int) { }.
    id :: @{Eq(a)}: a -> a.
    id = \\(x) -> x.
    class Use(a) { use :: a -> a. }.
    impl Use(Bool) { use = id. }.
    value = 1.
    """
    "missing impl fact 'Eq(Bool)'"

testSourceDiscardsFailedApplicationArgumentConstraints :: IO ()
testSourceDiscardsFailedApplicationArgumentConstraints = do
  result <-
    compileSourceWithPrelude
      defaultWarningSettings
      Nothing
      """
      class Need(a) { need :: a -> a. }.
      bad = 1 (\\(x) -> Need::need x).
      """
  assertSingleDiagnosticCode
    "failed application only reports E2006"
    "E2006"
    (compileErrors result)

testSourceDiscardsFailedApplicationFunctionConstraints :: IO ()
testSourceDiscardsFailedApplicationFunctionConstraints = do
  result <-
    compileSourceWithPrelude
      defaultWarningSettings
      Nothing
      """
      class Need(a) { need :: a -> a. }.
      bad = (\\(x) -> Need::need x) (1 True).
      """
  assertSingleDiagnosticCode
    "failed function operand only reports E2006"
    "E2006"
    (compileErrors result)

testSourceRejectsUnusedVariableConstraintWithBidirectionalContract :: IO ()
testSourceRejectsUnusedVariableConstraintWithBidirectionalContract = do
  result <- compileSource defaultWarningSettings """
  f :: @{Eq(a)}: Int -> Int.
  f = \\(x) -> x.
  """
  assertSingleDiagnosticCode
    "source unused variable constraint code"
    "E2009"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source unused variable constraint contract"
    "type-variable constrained signatures require every constrained variable to appear in the signature body"
    (compileErrors result)
