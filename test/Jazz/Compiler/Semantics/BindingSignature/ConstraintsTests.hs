{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Jazz.Compiler.Semantics.BindingSignature.ConstraintsTests
  ( constraintTests,
  )
where

import Jazz.Compiler.AST
  ( Expr (..),
    Statement (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Driver
  ( compileErrors,
    compileExpr,
    compileSource,
    compileSourceWithPrelude,
  )
import Jazz.Compiler.Name (mkIdentifier, sourceName)
import Jazz.Compiler.Semantics.BindingSignature.Shared
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    assertSingleDiagnosticPrimaryStart,
  )

constraintTests :: [NamedTest]
constraintTests =
  [ ("generic declarations reject invalid superclass, head, and primitive promises", testGenericDeclarationPromises),
    ("generic method definitions preserve independently quantified variables", testGenericMethodRigidity),
    ("generic instances check recursive element and container method calls", testGenericInstances),
    ("constructor parameters accept List and variable application", testConstructorParameters),
    ("constructor kinds reject invalid applications", testInvalidConstructorKinds),
    ("source pipeline accepts inert class and impl declarations", testSourceAcceptsCapabilityDeclarations),
    ("source pipeline accepts class method signature metadata", testSourceAcceptsClassMethodSignatureMetadata),
    ("source pipeline accepts method-local class signature variables", testSourceAcceptsMethodLocalClassSignatureVariables),
    ("source pipeline accepts constrained class method signatures", testSourceAcceptsConstrainedClassMethodSignatures),
    ("source pipeline rejects unknown named class method signatures", testSourceRejectsUnknownNamedClassMethodSignatures),
    ("source pipeline rejects duplicate class method signatures", testSourceRejectsDuplicateClassMethodSignatures),
    ("analyzer rejects duplicate class method metadata", testAnalyzerRejectsDuplicateClassMethodMetadata),
    ("source pipeline analyzes impl method binding metadata", testSourceAnalyzesImplMethodBindingMetadata),
    ("source pipeline rejects variable-target impl method bindings", testSourceRejectsVariableTargetImplMethodBindings),
    ("source pipeline rejects variable-target empty impl declarations", testSourceRejectsVariableTargetEmptyImplDeclarations),
    ("source pipeline rejects unknown named impl targets", testSourceRejectsUnknownNamedImplTargets),
    ("source pipeline rejects wrong-arity named impl targets", testSourceRejectsWrongArityNamedImplTargets),
    ("source pipeline instantiates unconstrained variables beside numeric constraints per use", testSourceInstantiatesUnconstrainedNumericBindingVariablesPerUse),
    ("source pipeline instantiates unconstrained variables beside equality constraints per use", testSourceInstantiatesUnconstrainedEqualityBindingVariablesPerUse),
    ("source pipeline infers equality class constraints for ordinary binding schemes", testSourceInfersEqualityClassConstraintsForOrdinaryBindingSchemes),
    ("source pipeline rejects missing inferred equality facts at use sites", testSourceRejectsMissingInferredEqualityFactAtUseSite),
    ("source pipeline rejects missing inferred equality facts through operator values", testSourceRejectsMissingInferredEqualityFactThroughOperatorValue),
    ("source pipeline rejects missing inferred equality facts through sections", testSourceRejectsMissingInferredEqualityFactThroughSection),
    ("source pipeline accepts primitive equality helpers without visible Equatable", testSourceAcceptsPrimitiveEqualityHelperWithoutVisibleEq),
    ("source pipeline rejects ambiguous inferred equality binding use", testSourceRejectsAmbiguousInferredEqualityBindingUse),
    ("source pipeline infers qualified method class constraints for ordinary binding schemes", testSourceInfersQualifiedMethodClassConstraintsForOrdinaryBindingSchemes),
    ("source pipeline resolves inferred method facts through aliases", testSourceResolvesInferredMethodFactsThroughAliases),
    ("source pipeline accepts expected-result method inference", testSourceAcceptsExpectedResultMethodInference),
    ("source pipeline rejects unpreserved higher-order qualified method inference", testSourceRejectsUnpreservedHigherOrderQualifiedMethodInference),
    ("source pipeline preserves inferred method constraints on signed bindings", testSourcePreservesInferredMethodConstraintsOnSignedBindings),
    ("source pipeline rejects undeclared equality constraints on signed bindings", testSourceRejectsUndeclaredEqualityConstraintsOnSignedBindings),
    ("source pipeline resolves concrete inferred method obligations before dropping them", testSourceResolvesConcreteInferredMethodObligationsBeforeDroppingThem),
    ("source pipeline resolves literal-range inferred method obligations before dropping them", testSourceResolvesLiteralRangeInferredMethodObligationsBeforeDroppingThem),
    ("source pipeline rejects ambiguous dropped literal-range method obligations", testSourceRejectsAmbiguousDroppedLiteralRangeMethodObligations),
    ("source pipeline preserves literal-range deferred method constraints", testSourcePreservesLiteralRangeDeferredMethodConstraints),
    ("source pipeline rejects ambiguous literal-range deferred method constraints", testSourceRejectsAmbiguousLiteralRangeDeferredMethodConstraints),
    ("source pipeline keeps nested helper inferred method obligations scoped", testSourceKeepsNestedHelperInferredMethodObligationsScoped),
    ("source pipeline preserves outer-scope local inferred method obligations", testSourcePreservesOuterScopeLocalInferredMethodObligations),
    ("source pipeline resolves concrete inferred equality obligations before dropping them", testSourceResolvesConcreteInferredEqualityObligationsBeforeDroppingThem),
    ("source pipeline checks inferred method obligations on expression statements", testSourceChecksInferredMethodObligationsOnExpressionStatements),
    ("source pipeline checks inferred equality obligations on expression statements", testSourceChecksInferredEqualityObligationsOnExpressionStatements),
    ("source pipeline rejects ambiguous inferred equality obligations on expression statements", testSourceRejectsAmbiguousInferredEqualityObligationsOnExpressionStatements),
    ("source pipeline checks inferred method obligations on monomorphic signed bindings", testSourceChecksInferredMethodObligationsOnMonomorphicSignedBindings),
    ("source pipeline rejects exact matches from non-target qualified method arguments", testSourceRejectsNonTargetQualifiedMethodExactMatch),
    ("source pipeline rejects callable equality before inferred class obligations", testSourceRejectsCallableEqualityBeforeInferredClassObligations),
    ("source pipeline rejects duplicate impl method bindings", testSourceRejectsDuplicateImplMethodBindings),
    ("source pipeline rejects non-binding impl body items", testSourceRejectsNonBindingImplBodyItem),
    ("source pipeline accepts single-target qualified method dispatch", testSourceAcceptsSingleTargetQualifiedMethodDispatch),
    ("source pipeline selects qualified method body by argument types", testSourceSelectsQualifiedMethodBodyByArgumentTypes),
    ("source pipeline uses static types for nested empty-list method selection", testSourceAcceptsNestedEmptyListExactQualifiedMethodSelection),
    ("source pipeline uses static types for constructor-wrapped nested empty-list method selection", testSourceAcceptsConstructorWrappedNestedEmptyListExactQualifiedMethodSelection),
    ("source pipeline uses static types for opaque nested empty-list method selection", testSourceAcceptsOpaqueNestedEmptyListExactQualifiedMethodSelection),
    ("source pipeline uses static types for opaque list application method selection", testSourceAcceptsOpaqueListApplicationExactQualifiedMethodSelection),
    ("source pipeline uses static types for block-produced nested empty-list method selection", testSourceAcceptsBlockProducedNestedEmptyListExactQualifiedMethodSelection),
    ("source pipeline uses static types for control-flow nested empty-list method selection", testSourceAcceptsControlFlowNestedEmptyListExactQualifiedMethodSelection),
    ("source pipeline selects qualified Float method body by argument types", testSourceSelectsQualifiedFloatMethodBodyByArgumentTypes),
    ("source pipeline selects qualified Float16 method body by argument types", testSourceSelectsQualifiedFloat16MethodBodyByArgumentTypes),
    ("source pipeline selects qualified Float32 method body by argument types", testSourceSelectsQualifiedFloat32MethodBodyByArgumentTypes),
    ("source pipeline selects qualified Float64 method body by argument types", testSourceSelectsQualifiedFloat64MethodBodyByArgumentTypes),
    ("source pipeline selects qualified method body through prefix dollar", testSourceSelectsQualifiedMethodBodyThroughPrefixDollar),
    ("source pipeline accepts same-impl qualified method body references", testSourceAcceptsSameImplQualifiedMethodBodyReferences),
    ("source pipeline uses impl signatures while checking method bodies", testSourceUsesImplSignaturesWhileCheckingMethodBodies),
    ("source pipeline uses impl signatures to contextualize method body lambdas", testSourceUsesImplSignaturesToContextualizeMethodBodyLambdas),
    ("source pipeline accepts higher-order qualified method signature", testSourceAcceptsHigherOrderQualifiedMethodSignature),
    ("source pipeline prefers visible binding over qualified method spine", testSourcePrefersVisibleBindingOverQualifiedMethodSpine),
    ("source pipeline applies substituted qualified method signature", testSourceRejectsQualifiedMethodSignatureMismatch),
    ("source pipeline rejects qualified method dispatch with no typed candidate", testSourceRejectsQualifiedMethodDispatchWithNoTypedCandidate),
    ("source pipeline rejects qualified impl method body mismatch", testSourceRejectsQualifiedImplMethodBodyMismatch),
    ("source pipeline rejects impl method before class method metadata", testSourceRejectsImplMethodBeforeClassMethodMetadata),
    ("source pipeline rejects qualified dispatch without class method metadata", testSourceRejectsQualifiedMethodMissingClassMethod),
    ("source pipeline rejects qualified dispatch without impl method body", testSourceRejectsQualifiedMethodMissingImplBody),
    ("source pipeline rejects deferred qualified method requirement without impl method body", testSourceRejectsDeferredQualifiedMethodRequirementMissingImplBody),
    ("source pipeline rejects ambiguous qualified method bodies", testSourceRejectsAmbiguousQualifiedMethodBodies),
    ("source pipeline rejects duplicate class declarations", testSourceRejectsDuplicateClassDeclarations),
    ("source pipeline rejects duplicate concrete impl declarations", testSourceRejectsDuplicateConcreteImplDeclarations),
    ("source pipeline rejects duplicate ADT impl declarations", testSourceRejectsDuplicateAdtImplDeclarations),
    ("compiler exposes imported qualified method bodies", testCompilerExposesImportedQualifiedMethodBodies),
    ("source pipeline accepts simple function signature", testSourceAcceptsSimpleFunctionSignature),
    ("source pipeline rejects concrete constrained signature without impl fact", testSourceRejectsConcreteConstrainedSignatureWithoutImplFact),
    ("source pipeline rejects unknown constrained signature constraint", testSourceRejectsUnknownConstrainedSignatureConstraint),
    ("source pipeline rejects wrong-arity constrained signature constraint", testSourceRejectsWrongArityConstrainedSignatureConstraint),
    ("source pipeline reports duplicate constrained signature constraints", testSourceRejectsDuplicateConstrainedSignatureConstraints),
    ("source pipeline instantiates equality constrained signatures per use", testSourceInstantiatesEqualityConstrainedSignaturePerUse),
    ("source pipeline accepts unconstrained variables beside explicit constraints", testSourceAcceptsUnconstrainedVariablesBesideExplicitConstraints),
    ("source pipeline preserves primitive constraints on variable constrained signatures", testSourcePreservesPrimitiveConstraintsOnVariableConstrainedSignatures),
    ("source pipeline rejects undeclared primitive constraints on signed bindings", testSourceRejectsUndeclaredPrimitiveConstraintsOnSignedBindings),
    ("source pipeline rejects undeclared class constraints on signed bindings", testSourceRejectsUndeclaredClassConstraintsOnSignedBindings),
    ("source pipeline preserves explicit constraints when primitive RHS has no quantified variables", testSourcePreservesExplicitConstraintsWhenPrimitiveRhsHasNoQuantifiedVariables),
    ("source pipeline preserves explicit Equatable impl checks for structural constraints", testSourcePreservesExplicitEqImplChecksForStructuralConstraints),
    ("source pipeline resolves deferred constraints in impl method bodies", testSourceResolvesDeferredConstraintsInImplMethodBodies),
    ("source pipeline discards failed application argument constraints", testSourceDiscardsFailedApplicationArgumentConstraints),
    ("source pipeline discards failed application function constraints", testSourceDiscardsFailedApplicationFunctionConstraints),
    ("source pipeline accepts explicit constraint-only type parameters", testSourceAcceptsConstraintOnlyTypeParameter)
  ]

testConstructorParameters :: IO ()
testConstructorParameters =
  assertSourceOkWithoutPrelude
    """
    data Wrapped f a = Wrapped f(a).
    keep :: Wrapped(List, Int) -> Wrapped(List, Int).
    keep = \\(x) -> x.
    keep (Wrapped [1, 2]).
    """

testInvalidConstructorKinds :: IO ()
testInvalidConstructorKinds =
  mapM_
    (`assertSourceSingleErrorContainsWithoutPrelude` "kind mismatch")
    [ "data Wrapped f a = Wrapped f(a). bad :: Wrapped(Int, Int). bad = 1.",
      "bad :: Int(Bool). bad = 1.",
      "data Infinite f = Infinite f(f).",
      "data Inconsistent f = Inconsistent f(Int) f(Int, Int).",
      "data Phantom f = Phantom. bad :: Phantom(List). bad = Phantom."
    ]

testSourceAcceptsCapabilityDeclarations :: IO ()
testSourceAcceptsCapabilityDeclarations =
  assertSourceOkWithoutPrelude
    """
    class Equatable(a) { }.
    impl Equatable(Int) { }.
    x :: Int.
    x = 1.
    x.
    """

testSourceAcceptsClassMethodSignatureMetadata :: IO ()
testSourceAcceptsClassMethodSignatureMetadata =
  assertSourceOkWithoutPrelude
    """
    class Equatable(a) {
    equals :: a -> a -> Bool.
    notEquals :: a -> a -> Bool.
    }.
    impl Equatable(Int) { equals = __kernel_equals. notEquals = \\(left, right) -> if left == right then False else True. }.
    x :: Int.
    x = 1.
    x.
    """

testSourceAcceptsMethodLocalClassSignatureVariables :: IO ()
testSourceAcceptsMethodLocalClassSignatureVariables =
  assertSourceOkWithoutPrelude
    "class Transforming(f) { transform :: (a -> b) -> f(a) -> f(b). }. 0."

testSourceAcceptsConstrainedClassMethodSignatures :: IO ()
testSourceAcceptsConstrainedClassMethodSignatures =
  assertSourceOkWithoutPrelude
    """
    class Need(a) { }.
    class C(a) { m :: @{Need(b)}: a -> b -> Bool. }.
    0.
    """

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
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { equals :: Int. equals :: Bool. }.
    x = 1.
    """
    "duplicate method signature 'equals'"

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
    program =
      case ( loweredProgram "class Equatable(a) { equals :: Int. }. 1.",
             loweredProgram "class Equatable(a) { equals :: Bool. }."
           ) of
        ( EBlock blockNode (SClass classNode className parameters methods prerequisites defaults : statements),
          EBlock _ (SClass _ _ _ duplicateMethods _ _ : _)
          ) ->
            EBlock blockNode (SClass classNode className parameters (methods <> duplicateMethods) prerequisites defaults : statements)
        (firstProgram, secondProgram) ->
          error ("expected class declaration blocks, got " <> show (firstProgram, secondProgram))

testSourceAnalyzesImplMethodBindingMetadata :: IO ()
testSourceAnalyzesImplMethodBindingMetadata = do
  assertSourceOkWithoutPrelude
    """
    class Equatable(a) {
    equals :: a -> a -> Bool.
    }.
    impl Equatable(Int) {
    equals = __kernel_equals.
    }.
    x :: Int.
    x = 1.
    x.
    """
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) {
    equals :: a -> a -> Bool.
    }.
    impl Equatable(Int) {
    equals = missingImplRuntime.
    }.
    x :: Int.
    x = 1.
    x.
    """
    "unbound variable 'missingImplRuntime'"

testSourceRejectsVariableTargetImplMethodBindings :: IO ()
testSourceRejectsVariableTargetImplMethodBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    impl Equatable(a) { equals = 1. }.
    x = 1.
    """
    "constructor-headed impl target"

testSourceRejectsVariableTargetEmptyImplDeclarations :: IO ()
testSourceRejectsVariableTargetEmptyImplDeclarations =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    impl Equatable(a) { }.
    x = 1.
    """
    "constructor-headed impl target"

testSourceRejectsUnknownNamedImplTargets :: IO ()
testSourceRejectsUnknownNamedImplTargets =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    impl Equatable(Unknown(Char)) { }.
    x = 1.
    """
    "unknown named type 'Unknown'"

testSourceRejectsWrongArityNamedImplTargets :: IO ()
testSourceRejectsWrongArityNamedImplTargets =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    data Box a = Box a.
    class Equatable(a) { }.
    impl Equatable(Box(Int, Bool)) { }.
    x = 1.
    """
    "type 'Box' expects 1 argument(s), found 2"

testSourceInstantiatesUnconstrainedNumericBindingVariablesPerUse :: IO ()
testSourceInstantiatesUnconstrainedNumericBindingVariablesPerUse =
  assertSourceOk
    """
    f = \\(x, y) -> (x + x, y).
    a = f 1 True.
    b = f 2 3.
    """

testSourceInstantiatesUnconstrainedEqualityBindingVariablesPerUse :: IO ()
testSourceInstantiatesUnconstrainedEqualityBindingVariablesPerUse =
  assertSourceOk
    """
    f = \\(x, y) -> (x == x, y).
    a = f 1 True.
    b = f 2 3.
    """

testSourceInfersEqualityClassConstraintsForOrdinaryBindingSchemes :: IO ()
testSourceInfersEqualityClassConstraintsForOrdinaryBindingSchemes =
  assertSourceOkWithoutPrelude
    """
    class Equatable(a) { equals :: a -> a -> Bool. }.
    impl Equatable(Int) { equals = __kernel_equals. }.
    impl Equatable(Bool) { equals = __kernel_equals. }.
    same = \\(left, right) -> left == right.
    intResult = same 1 1.
    boolResult = same True False.
    """

testSourceRejectsMissingInferredEqualityFactAtUseSite :: IO ()
testSourceRejectsMissingInferredEqualityFactAtUseSite =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { equals :: a -> a -> Bool. }.
    impl Equatable(Int) { equals = __kernel_equals. }.
    same = \\(left, right) -> left == right.
    intResult = same 1 1.
    bad = same True False.
    """
    "missing impl fact 'Equatable(Bool)'"

testSourceRejectsMissingInferredEqualityFactThroughOperatorValue :: IO ()
testSourceRejectsMissingInferredEqualityFactThroughOperatorValue =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { equals :: a -> a -> Bool. }.
    impl Equatable(Int) { equals = __kernel_equals. }.
    same = (==).
    intResult = same 1 1.
    bad = same True False.
    """
    "missing impl fact 'Equatable(Bool)'"

testSourceRejectsMissingInferredEqualityFactThroughSection :: IO ()
testSourceRejectsMissingInferredEqualityFactThroughSection =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { equals :: a -> a -> Bool. }.
    impl Equatable(Int) { equals = __kernel_equals. }.
    same = \\(right) -> (== right).
    intResult = same 1 1.
    bad = same True False.
    """
    "missing impl fact 'Equatable(Bool)'"

testSourceAcceptsPrimitiveEqualityHelperWithoutVisibleEq :: IO ()
testSourceAcceptsPrimitiveEqualityHelperWithoutVisibleEq =
  assertSourceOkWithoutPrelude
    """
    same = \\(x) -> __kernel_equals x x.
    ok = same 1.
    """

testSourceRejectsAmbiguousInferredEqualityBindingUse :: IO ()
testSourceRejectsAmbiguousInferredEqualityBindingUse =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
      class Equatable(a) { equals :: a -> a -> Bool. }.
      ambiguous = \\(x) -> x == x.
      ambiguous.
      """
    )
    "ambiguous qualified method body 'Equatable::equals'"

testSourceInfersQualifiedMethodClassConstraintsForOrdinaryBindingSchemes :: IO ()
testSourceInfersQualifiedMethodClassConstraintsForOrdinaryBindingSchemes =
  assertSourceOkWithoutPrelude
    ( """
      class Equatable(a) {
      equals :: a -> a -> Bool.
      }.
      impl Equatable(Int) {
      equals = __kernel_equals.
      }.
      impl Equatable(Bool) {
      equals = __kernel_equals.
      }.
      same = \\(left, right) -> Equatable::equals left right.
      intResult = same 1 1.
      boolResult = same True False.
      """
    )

testSourceResolvesInferredMethodFactsThroughAliases :: IO ()
testSourceResolvesInferredMethodFactsThroughAliases =
  assertSourceOkWithoutPrelude
    ( """
      class Equatable(a) {
      equals :: a -> a -> Bool.
      }.
      impl Equatable(Float) {
      equals = __kernel_equals.
      }.
      candidate :: Float64.
      candidate = 1.5.
      same = \\(x) -> Equatable::equals x x.
      result = same candidate.
      """
    )

testSourceAcceptsExpectedResultMethodInference :: IO ()
testSourceAcceptsExpectedResultMethodInference =
  assertSourceOkWithoutPrelude
    ( """
      class Make(a) {
      make :: Int -> a.
      }.
      impl Make(Int) {
      make = \\(candidate) -> candidate.
      }.
      impl Make(Bool) {
      make = \\(candidate) -> True.
      }.
      x :: Int.
      x = Make::make 0.
      """
    )

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
      f :: @{C(a)}: a -> Bool.
      f = \\(x) -> C::m x.
      result = f 1.
      """
    )
    "missing impl fact 'C(Int)'"

testSourceRejectsUndeclaredEqualityConstraintsOnSignedBindings :: IO ()
testSourceRejectsUndeclaredEqualityConstraintsOnSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
      class Equatable(a) { equals :: a -> a -> Bool. }.
      class C(a) { }.
      same :: @{C(a)}: a -> a -> Bool.
      same = \\(x, y) -> x == y.
      """
    )
    "does not declare required constraint 'Equatable"

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
      class Equatable(a) { equals :: a -> a -> Bool. }.
      impl Equatable(Int) { equals = __kernel_equals. }.
      result = (\\(x) -> x == x) True.
      """
    )
    "missing impl fact 'Equatable(Bool)'"

testSourceChecksInferredMethodObligationsOnExpressionStatements :: IO ()
testSourceChecksInferredMethodObligationsOnExpressionStatements =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
      class C(a) {
      m :: a -> Bool.
      }.
      (\\(x) -> C::m x) 1.
      """
    )
    "missing impl fact 'C(Int)'"

testSourceChecksInferredEqualityObligationsOnExpressionStatements :: IO ()
testSourceChecksInferredEqualityObligationsOnExpressionStatements =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
      class Equatable(a) { equals :: a -> a -> Bool. }.
      impl Equatable(Int) { equals = __kernel_equals. }.
      (\\(x) -> x == x) True.
      """
    )
    "missing impl fact 'Equatable(Bool)'"

testSourceRejectsAmbiguousInferredEqualityObligationsOnExpressionStatements :: IO ()
testSourceRejectsAmbiguousInferredEqualityObligationsOnExpressionStatements =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
      class Equatable(a) { equals :: a -> a -> Bool. }.
      \\(x) -> x == x.
      """
    )
    "ambiguous qualified method body 'Equatable::equals'"

testSourceChecksInferredMethodObligationsOnMonomorphicSignedBindings :: IO ()
testSourceChecksInferredMethodObligationsOnMonomorphicSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
      class C(a) {
      m :: a -> Bool.
      }.
      result :: Bool.
      result = (\\(x) -> C::m x) 1.
      """
    )
    "missing impl fact 'C(Int)'"

testSourceRejectsNonTargetQualifiedMethodExactMatch :: IO ()
testSourceRejectsNonTargetQualifiedMethodExactMatch =
  assertSourceSingleErrorContainsWithoutPrelude
    ( """
      class Flag(a) {
      flag :: Int -> Bool.
      }.
      impl Flag(Int) {
      flag = \\(candidate) -> True.
      }.
      impl Flag(Bool) {
      flag = \\(candidate) -> False.
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
    f :: Int -> Int.
    f = \\(x) -> x.
    bad = f == f.
    """
    "missing impl fact 'Equatable(Int -> Int)'"

testSourceRejectsDuplicateImplMethodBindings :: IO ()
testSourceRejectsDuplicateImplMethodBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    impl Equatable(Int) { equals = 1. equals = 2. }.
    x = 1.
    """
    "duplicate method binding 'equals'"

testSourceRejectsNonBindingImplBodyItem :: IO ()
testSourceRejectsNonBindingImplBodyItem =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    impl Equatable(Int) { equals :: Int. }.
    x = 1.
    """
    "ordinary method binding"

testSourceAcceptsSingleTargetQualifiedMethodDispatch :: IO ()
testSourceAcceptsSingleTargetQualifiedMethodDispatch =
  assertSourceOkWithoutPrelude
    ( qualifiedEqSource
        <> """
           result :: Bool.
           result = Equatable::equals 1 1.
           result.
           """
    )

testSourceSelectsQualifiedMethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedMethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( qualifiedEqSource
        <> """
           impl Equatable(Bool) {
           equals = __kernel_equals.
           }.
           result :: Bool.
           result = Equatable::equals True False.
           result.
           """
    )

testSourceAcceptsNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceAcceptsNestedEmptyListExactQualifiedMethodSelection =
  assertSourceOkWithoutPrelude
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

testSourceAcceptsConstructorWrappedNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceAcceptsConstructorWrappedNestedEmptyListExactQualifiedMethodSelection =
  assertSourceOkWithoutPrelude
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

testSourceAcceptsOpaqueNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceAcceptsOpaqueNestedEmptyListExactQualifiedMethodSelection =
  assertSourceOkWithoutPrelude
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

testSourceAcceptsOpaqueListApplicationExactQualifiedMethodSelection :: IO ()
testSourceAcceptsOpaqueListApplicationExactQualifiedMethodSelection =
  assertSourceOkWithoutPrelude
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

testSourceAcceptsBlockProducedNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceAcceptsBlockProducedNestedEmptyListExactQualifiedMethodSelection =
  assertSourceOkWithoutPrelude
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

testSourceAcceptsControlFlowNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceAcceptsControlFlowNestedEmptyListExactQualifiedMethodSelection =
  assertSourceOkWithoutPrelude
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

testSourceSelectsQualifiedFloatMethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloatMethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( """
      class Equatable(a) {
      equals :: a -> a -> Bool.
      }.
      impl Equatable(Float) {
      equals = __kernel_equals.
      }.
      left :: Float.
      left = 1.5.
      right :: Float.
      right = 2.25.
      result :: Bool.
      result = Equatable::equals left right.
      result.
      """
    )

testSourceSelectsQualifiedFloat16MethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloat16MethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( """
      class Equatable(a) {
      equals :: a -> a -> Bool.
      }.
      impl Equatable(Float16) {
      equals = __kernel_equals.
      }.
      left :: Float16.
      left = 1.5.
      right :: Float16.
      right = 2.25.
      result :: Bool.
      result = Equatable::equals left right.
      result.
      """
    )

testSourceSelectsQualifiedFloat32MethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloat32MethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( """
      class Equatable(a) {
      equals :: a -> a -> Bool.
      }.
      impl Equatable(Float32) {
      equals = __kernel_equals.
      }.
      left :: Float32.
      left = 1.5.
      right :: Float32.
      right = 2.25.
      result :: Bool.
      result = Equatable::equals left right.
      result.
      """
    )

testSourceSelectsQualifiedFloat64MethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloat64MethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( """
      class Equatable(a) {
      equals :: a -> a -> Bool.
      }.
      impl Equatable(Float64) {
      equals = __kernel_equals.
      }.
      left :: Float64.
      left = 1.5.
      right :: Float64.
      right = 2.25.
      result :: Bool.
      result = Equatable::equals left right.
      result.
      """
    )

testSourceSelectsQualifiedMethodBodyThroughPrefixDollar :: IO ()
testSourceSelectsQualifiedMethodBodyThroughPrefixDollar =
  assertSourceOk
    ( """
      class Choice(a) {
      pick :: a -> Bool.
      }.
      impl Choice(Int) {
      pick = \\(candidate) -> True.
      }.
      impl Choice(Bool) {
      pick = \\(candidate) -> False.
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
      class Equatable(a) {
      equals :: a -> a -> Bool.
      notEquals :: a -> a -> Bool.
      }.
      impl Equatable(Int) {
      equals = __kernel_equals.
      notEquals = \\(left, right) -> __kernel_equals (Equatable::equals left right) False.
      }.
      result :: Bool.
      result = Equatable::notEquals 1 2.
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
      check = \\(candidate) -> True.
      notCheck = \\(candidate) -> __kernel_equals (Check::check candidate) False.
      }.
      impl Check(Bool) {
      check = \\(candidate) -> False.
      notCheck = \\(candidate) -> __kernel_equals (Check::check candidate) False.
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
      n = \\(candidate) -> True.
      }.
      impl D(Bool) {
      n = \\(candidate) -> False.
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
      result = Apply::apply @Int (\\(item) -> __kernel_add item 1).
      result.
      """
    )

testSourcePrefersVisibleBindingOverQualifiedMethodSpine :: IO ()
testSourcePrefersVisibleBindingOverQualifiedMethodSpine = do
  result <-
    compileExpr
      defaultWarningSettings
      program
  assertEqual "binding-precedence compile errors" [] (compileErrors result)
  where
    qualifiedHelper = sourceName (mkIdentifier "Equatable::helper")
    program =
      case loweredProgram "class Equatable(a) { }. helper = \\(item) -> item. helper 1." of
        EBlock blockNode [classStatement, SLet bindingNode _ body, SExpr expressionNode (EApply applicationNode (EVar variableNode _) argument)] ->
          EBlock
            blockNode
            [ classStatement,
              SLet bindingNode qualifiedHelper body,
              SExpr expressionNode (EApply applicationNode (EVar variableNode qualifiedHelper) argument)
            ]
        expression -> error ("expected qualified helper fixture shape, got " <> show expression)

testSourceRejectsQualifiedMethodSignatureMismatch :: IO ()
testSourceRejectsQualifiedMethodSignatureMismatch =
  assertSourceSingleErrorContainsWithoutPrelude
    ( qualifiedEqSource
        <> """
           result = Equatable::equals 1 True.
           result.
           """
    )
    "cannot apply function of type Int -> Bool to argument of type Bool"

testSourceRejectsQualifiedMethodDispatchWithNoTypedCandidate :: IO ()
testSourceRejectsQualifiedMethodDispatchWithNoTypedCandidate =
  assertSourceSingleErrorContainsWithoutPrelude
    ( qualifiedEqSource
        <> """
           impl Equatable(Bool) {
           equals = __kernel_equals.
           }.
           result = Equatable::equals 1 False.
           result.
           """
    )
    "cannot apply function of type Int -> Bool to argument of type Bool"

testSourceRejectsQualifiedImplMethodBodyMismatch :: IO ()
testSourceRejectsQualifiedImplMethodBodyMismatch =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) {
    equals :: a -> a -> Bool.
    }.
    impl Equatable(Int) {
    equals = 1.
    }.
    result = Equatable::equals 1 1.
    result.
    """
    "impl method 'Equatable::equals' declared as Int -> Int -> Bool but inferred as Int"

testSourceRejectsImplMethodBeforeClassMethodMetadata :: IO ()
testSourceRejectsImplMethodBeforeClassMethodMetadata = do
  result <-
    compileSourceWithPrelude
      defaultWarningSettings
      Nothing
      """
      impl Equatable(Int) {
      equals = 1.
      }.
      class Equatable(a) {
      equals :: a -> a -> Bool.
      }.
      """
  assertSingleDiagnosticContains
    "impl-before-class method metadata"
    "missing class declaration 'Equatable'"
    (compileErrors result)
  assertSingleDiagnosticPrimaryStart
    "impl-before-class method metadata span"
    (SourceSpan 1 1)
    (compileErrors result)

testSourceRejectsQualifiedMethodMissingClassMethod :: IO ()
testSourceRejectsQualifiedMethodMissingClassMethod =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    impl Equatable(Int) {
    equals = __kernel_equals.
    }.
    """
    "class method metadata for 'Equatable::equals' must be declared before impl method body"

testSourceRejectsQualifiedMethodMissingImplBody :: IO ()
testSourceRejectsQualifiedMethodMissingImplBody =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) {
    equals :: a -> a -> Bool.
    }.
    impl Equatable(Int) { }.
    """
    "missing impl method body 'Equatable::equals'"

testSourceRejectsDeferredQualifiedMethodRequirementMissingImplBody :: IO ()
testSourceRejectsDeferredQualifiedMethodRequirementMissingImplBody =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) {
    equals :: a -> a -> Bool.
    }.
    same = \\(x) -> Equatable::equals x x.
    result = same 1.
    """
    "missing impl fact 'Equatable(Int)'"

testSourceRejectsAmbiguousQualifiedMethodBodies :: IO ()
testSourceRejectsAmbiguousQualifiedMethodBodies =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Classify(a) {
    classify :: Int -> Bool.
    }.
    impl Classify(Int) {
    classify = \\(candidate) -> __kernel_equals candidate 1.
    }.
    impl Classify(Bool) {
    classify = \\(candidate) -> __kernel_equals candidate 2.
    }.
    result = Classify::classify 1.
    result.
    """
    "ambiguous qualified method body 'Classify::classify'"

testSourceRejectsDuplicateClassDeclarations :: IO ()
testSourceRejectsDuplicateClassDeclarations =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    class Equatable(b) { }.
    x = 1.
    """
    "E1004"

testSourceRejectsDuplicateConcreteImplDeclarations :: IO ()
testSourceRejectsDuplicateConcreteImplDeclarations = do
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    impl Equatable(Int) { }.
    impl Equatable(Int) { }.
    x = 1.
    """
    "E1005"
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    impl Equatable(Float) { }.
    impl Equatable(Float) { }.
    x = 1.
    """
    "E1005"
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    impl Equatable(Float64) { }.
    impl Equatable(Float64) { }.
    x = 1.
    """
    "E1005"

testSourceRejectsDuplicateAdtImplDeclarations :: IO ()
testSourceRejectsDuplicateAdtImplDeclarations = do
  assertSourceSingleErrorContainsWithoutPrelude
    """
    data Color = Red.
    class Equatable(a) { }.
    impl Equatable(Color) { }.
    impl Equatable(Color) { }.
    x = 1.
    """
    "E1005"
  assertSourceSingleErrorContainsWithoutPrelude
    """
    data Box a = Box a.
    class Equatable(a) { }.
    impl Equatable(Box(Int)) { }.
    impl Equatable(Box(Int)) { }.
    x = 1.
    """
    "E1005"

testCompilerExposesImportedQualifiedMethodBodies :: IO ()
testCompilerExposesImportedQualifiedMethodBodies = do
  result <- compileExpr defaultWarningSettings importedQualifiedMethodFactsProgram
  assertEqual "imported qualified method compile errors" [] (compileErrors result)

testSourceAcceptsSimpleFunctionSignature :: IO ()
testSourceAcceptsSimpleFunctionSignature =
  assertSourceOk
    """
    f :: Int -> Int.
    f = (+ 1).
    """

testSourceRejectsConcreteConstrainedSignatureWithoutImplFact :: IO ()
testSourceRejectsConcreteConstrainedSignatureWithoutImplFact =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    x :: @{Equatable(Int)}: Int.
    x = 1.
    """
    "missing impl fact 'Equatable(Int)'"

testSourceRejectsUnknownConstrainedSignatureConstraint :: IO ()
testSourceRejectsUnknownConstrainedSignatureConstraint =
  assertSourceSingleErrorContains
    """
    x :: @{Unknown(Int)}: Int.
    x = 1.
    """
    "E2009"

testSourceRejectsWrongArityConstrainedSignatureConstraint :: IO ()
testSourceRejectsWrongArityConstrainedSignatureConstraint =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    impl Equatable(Int) { }.
    x :: @{Equatable(Int, Bool)}: Int.
    x = 1.
    """
    "constraint 'Equatable' expects 1 argument(s), got 2"

testSourceRejectsDuplicateConstrainedSignatureConstraints :: IO ()
testSourceRejectsDuplicateConstrainedSignatureConstraints = do
  result <-
    compileSource
      defaultWarningSettings
      """
      f :: @{Equatable(a), Equatable(a)}: a -> a.
      f = \\(x) -> x.
      """
  assertSingleDiagnosticCode
    "source duplicate constrained signature code"
    "E2009"
    (compileErrors result)
  assertSingleDiagnosticContains
    "source duplicate constrained signature text"
    "duplicate constraint 'Equatable'"
    (compileErrors result)

testSourceInstantiatesEqualityConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesEqualityConstrainedSignaturePerUse =
  assertSourceOkWithoutPrelude
    """
    class Equatable(a) { equals :: a -> a -> Bool. }.
    impl Equatable(Int) { equals = __kernel_equals. }.
    impl Equatable(Bool) { equals = __kernel_equals. }.
    same :: @{Equatable(a)}: a -> a -> Bool.
    same = \\(x, y) -> x == y.
    intValue = same 1 2.
    boolValue = same True False.
    """

testSourceAcceptsUnconstrainedVariablesBesideExplicitConstraints :: IO ()
testSourceAcceptsUnconstrainedVariablesBesideExplicitConstraints =
  assertSourceOk
    """
    choose :: @{Equatable(a)}: a -> b -> a.
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
    addSelf = \\(x) -> __kernel_add x x.
    bad = addSelf True.
    """
    "cannot apply function"

testSourceRejectsUndeclaredPrimitiveConstraintsOnSignedBindings :: IO ()
testSourceRejectsUndeclaredPrimitiveConstraintsOnSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    bad :: a -> a.
    bad = \\(x) -> __kernel_add x x.
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
    addSelf = \\(x) -> __kernel_add x x.
    good = addSelf 1.
    """
    "missing impl fact 'Num(Int)'"

testSourcePreservesExplicitEqImplChecksForStructuralConstraints :: IO ()
testSourcePreservesExplicitEqImplChecksForStructuralConstraints =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    xs :: @{Equatable([Int])}: [Int].
    xs = [1].
    """
    "missing impl fact 'Equatable([Int])'"

testSourceResolvesDeferredConstraintsInImplMethodBodies :: IO ()
testSourceResolvesDeferredConstraintsInImplMethodBodies =
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Equatable(a) { }.
    impl Equatable(Int) { }.
    id :: @{Equatable(a)}: a -> a.
    id = \\(x) -> x.
    class Use(a) { use :: a -> a. }.
    impl Use(Bool) { use = id. }.
    candidate = 1.
    """
    "missing impl fact 'Equatable(Bool)'"

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

testSourceAcceptsConstraintOnlyTypeParameter :: IO ()
testSourceAcceptsConstraintOnlyTypeParameter =
  assertSourceOkWithoutPrelude
    """
    class C(a) { }.
    impl C(Int) { }.
    f :: @{C(a)}: Int -> Int.
    f = \\(x) -> x.
    f @Int 1.
    """

testGenericInstances :: IO ()
testGenericInstances =
  assertSourceOkWithoutPrelude
    """
    class Same(a) { same :: a -> a -> Bool. }.
    impl Same(Int) { same = __kernel_equals. }.
    impl @{Same(a)}: Same([a]) {
      same = \\(left, right) -> case (left, right) {
        | ([], []) -> True
        | ([x | xs], [y | ys]) -> if Same::same x y then Same::same xs ys else False
        | _ -> False
      }.
    }.
    check = \\(x, y) -> Same::same x y.
    (check [[1]] [[1]], check [[1]] [[2]]).
    """

testGenericMethodRigidity :: IO ()
testGenericMethodRigidity = do
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Transforming(f) { transform :: (a -> b) -> f(a) -> f(b). }.
    impl Transforming(List) { transform = \\(change, xs) -> xs. }.
    """
    "impl method"
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Keeping(t) { keep :: t -> t. }.
    impl Keeping([a]) { keep = \\(xs) -> [True]. }.
    """
    "impl method"

testGenericDeclarationPromises :: IO ()
testGenericDeclarationPromises = do
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Parent(a) { parent :: a -> Bool. }.
    class @{Parent(a)}: Child(a) { child :: a -> Bool. }.
    impl Child(Int) { child = \\(x) -> True. }.
    """
    "missing impl fact 'Parent(Int)'"
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class @{Loop(a)}: Loop(a) { loop :: a -> Bool. }.
    """
    "cyclic superclass"
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Keeping(a) { keep :: a -> a. keep = \\(x) -> True. }.
    """
    "impl method"
  assertSourceSingleErrorContainsWithoutPrelude
    """
    data Box a = Box a.
    class Adding(a) { add :: a -> a. }.
    impl Adding(Box(a)) { add = \\(Box x) -> Box (__kernel_add x x). }.
    """
    "does not declare required primitive constraint"
  assertSourceSingleErrorContainsWithoutPrelude
    """
    class Testing(a) { test :: a -> Bool. test = \\(x) -> __kernel_equals x x. }.
    """
    "does not declare required primitive constraint"
