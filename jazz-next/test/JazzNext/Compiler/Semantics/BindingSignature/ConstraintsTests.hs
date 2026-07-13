{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.BindingSignature.ConstraintsTests
  ( constraintTests
  ) where


import qualified Data.Set as Set
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( ClassMethodSignature (..),
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
import JazzNext.Compiler.TypeInference.Types
  ( ExpressionType (..),
    TypeScheme (..),
    emptyScopeCapabilityFacts
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    RunResult (..),
    compileExpr,
    compileSource,
    compileSourceWithPrelude,
    runSourceWithPrelude
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
    , ("source pipeline rejects unused variable constraint with bidirectional contract", testSourceRejectsUnusedVariableConstraintWithBidirectionalContract)
  ]

testSourceAcceptsCapabilityDeclarations :: IO ()
testSourceAcceptsCapabilityDeclarations =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nx :: Int.\nx = 1.\nx."

testSourceAcceptsClassMethodSignatureMetadata :: IO ()
testSourceAcceptsClassMethodSignatureMetadata =
  assertSourceOkWithoutPrelude "class Eq(a) {\nequals :: a -> a -> Bool.\nnotEquals :: a -> a -> Bool.\n}.\nimpl Eq(Int) { }.\nx :: Int.\nx = 1.\nx."

testSourceRejectsMethodLocalClassSignatureVariables :: IO ()
testSourceRejectsMethodLocalClassSignatureVariables =
  assertSourceSingleErrorContainsWithoutPrelude
    "class C(a) { f :: b -> b. }."
    "method-local type variable 'b'"

testSourceRejectsConstrainedClassMethodSignatures :: IO ()
testSourceRejectsConstrainedClassMethodSignatures =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Need(a) { }.\nclass C(a) { m :: @{Need(a)}: a -> Bool. }.\n0."
    "invalid or unsupported class method signature for 'C::m'"

testSourceRejectsUnknownNamedClassMethodSignatures :: IO ()
testSourceRejectsUnknownNamedClassMethodSignatures =
  assertSourceSingleErrorContainsWithoutPrelude
    "class C(a) { f :: Unknown -> a. }.\nx = 1."
    "unknown named type 'Unknown'"

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

testSourceRejectsUnknownNamedImplTargets :: IO ()
testSourceRejectsUnknownNamedImplTargets =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) { }.\nimpl Eq(Unknown(Char)) { }.\nx = 1."
    "unknown named type 'Unknown'"

testSourceRejectsWrongArityNamedImplTargets :: IO ()
testSourceRejectsWrongArityNamedImplTargets =
  assertSourceSingleErrorContainsWithoutPrelude
    "data Box a = Box a.\nclass Eq(a) { }.\nimpl Eq(Box(Int, Bool)) { }.\nx = 1."
    "type 'Box' expects 1 argument(s), found 2"

testSourceInstantiatesUnconstrainedNumericBindingVariablesPerUse :: IO ()
testSourceInstantiatesUnconstrainedNumericBindingVariablesPerUse =
  assertSourceOk "f = \\(x) -> \\(y) -> (x + x, y).\na = f 1 True.\nb = f 2 3."

testSourceInstantiatesUnconstrainedEqualityBindingVariablesPerUse :: IO ()
testSourceInstantiatesUnconstrainedEqualityBindingVariablesPerUse =
  assertSourceOk "f = \\(x) -> \\(y) -> (x == x, y).\na = f 1 True.\nb = f 2 3."

testSourceInfersEqualityClassConstraintsForOrdinaryBindingSchemes :: IO ()
testSourceInfersEqualityClassConstraintsForOrdinaryBindingSchemes =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nimpl Eq(Bool) { }.\nsame = \\(left) -> \\(right) -> left == right.\nintResult = same 1 1.\nboolResult = same True False."

testSourceRejectsMissingInferredEqualityFactAtUseSite :: IO ()
testSourceRejectsMissingInferredEqualityFactAtUseSite =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) { }.\nimpl Eq(Int) { }.\nsame = \\(left) -> \\(right) -> left == right.\nintResult = same 1 1.\nbad = same True False."
    "missing impl fact 'Eq(Bool)'"

testSourceRejectsMissingInferredEqualityFactThroughOperatorValue :: IO ()
testSourceRejectsMissingInferredEqualityFactThroughOperatorValue =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) { }.\nimpl Eq(Int) { }.\nsame = (==).\nintResult = same 1 1.\nbad = same True False."
    "missing impl fact 'Eq(Bool)'"

testSourceRejectsMissingInferredEqualityFactThroughSection :: IO ()
testSourceRejectsMissingInferredEqualityFactThroughSection =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) { }.\nimpl Eq(Int) { }.\nsame = \\(right) -> (== right).\nintResult = same 1 1.\nbad = same True False."
    "missing impl fact 'Eq(Bool)'"

testSourceAcceptsPrimitiveEqualityHelperWithoutVisibleEq :: IO ()
testSourceAcceptsPrimitiveEqualityHelperWithoutVisibleEq =
  assertSourceOkWithoutPrelude "same = \\(x) -> x == x.\nok = same 1."

testSourceRejectsAmbiguousInferredEqualityBindingUse :: IO ()
testSourceRejectsAmbiguousInferredEqualityBindingUse =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class Eq(a) { }.\n"
        <> "ambiguous = \\(x) -> x == x.\n"
        <> "ambiguous."
    )
    "ambiguous/defaulting inferred constraint 'Eq"

testSourceInfersQualifiedMethodClassConstraintsForOrdinaryBindingSchemes :: IO ()
testSourceInfersQualifiedMethodClassConstraintsForOrdinaryBindingSchemes =
  assertSourceOkWithoutPrelude
    ( "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\n"
        <> "impl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
        <> "impl Eq(Bool) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
        <> "same = \\(left) -> \\(right) -> Eq::equals left right.\n"
        <> "intResult = same 1 1.\n"
        <> "boolResult = same True False."
    )

testSourceResolvesInferredMethodFactsThroughAliases :: IO ()
testSourceResolvesInferredMethodFactsThroughAliases =
  assertSourceOkWithoutPrelude
    ( "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\n"
        <> "impl Eq(Float) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
        <> "value :: Float64.\nvalue = 1.5.\n"
        <> "same = \\(x) -> Eq::equals x x.\n"
        <> "result = same value."
    )

testSourceRejectsResultOnlyQualifiedMethodInference :: IO ()
testSourceRejectsResultOnlyQualifiedMethodInference =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class Make(a) {\nmake :: Int -> a.\n}.\n"
        <> "impl Make(Int) {\nmake = \\(value) -> value.\n}.\n"
        <> "impl Make(Bool) {\nmake = \\(value) -> True.\n}.\n"
        <> "x :: Int.\n"
        <> "x = Make::make 0."
    )
    "ambiguous qualified method body"

testSourceRejectsUnpreservedHigherOrderQualifiedMethodInference :: IO ()
testSourceRejectsUnpreservedHigherOrderQualifiedMethodInference =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class C(a) {\nm :: (a -> a) -> Bool.\n}.\n"
        <> "impl C(Int) {\nm = \\(f) -> True.\n}.\n"
        <> "impl C(Bool) {\nm = \\(f) -> False.\n}.\n"
        <> "f = \\(x) -> x.\n"
        <> "result = C::m f."
    )
    "ambiguous qualified method body"

testSourcePreservesInferredMethodConstraintsOnSignedBindings :: IO ()
testSourcePreservesInferredMethodConstraintsOnSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int) { }.\n"
        <> "f :: @{C(a)}: a -> Bool.\n"
        <> "f = \\(x) -> C::m x.\n"
        <> "result = f 1."
    )
    "missing impl method body 'C::m'"

testSourceRejectsUndeclaredEqualityConstraintsOnSignedBindings :: IO ()
testSourceRejectsUndeclaredEqualityConstraintsOnSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class Eq(a) { }.\n"
        <> "class C(a) { }.\n"
        <> "same :: @{C(a)}: a -> a -> Bool.\n"
        <> "same = \\(x) -> \\(y) -> x == y."
    )
    "does not declare required constraint 'Eq"

testSourceResolvesConcreteInferredMethodObligationsBeforeDroppingThem :: IO ()
testSourceResolvesConcreteInferredMethodObligationsBeforeDroppingThem =
  assertSourceOkWithoutPrelude
    ( "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int) {\nm = \\(x) -> True.\n}.\n"
        <> "result = (\\(x) -> C::m x) 1."
    )

testSourceResolvesLiteralRangeInferredMethodObligationsBeforeDroppingThem :: IO ()
testSourceResolvesLiteralRangeInferredMethodObligationsBeforeDroppingThem =
  assertSourceOkWithoutPrelude
    ( "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int8) {\nm = \\(x) -> True.\n}.\n"
        <> "result = (\\(x) -> C::m x) 1."
    )

testSourceRejectsAmbiguousDroppedLiteralRangeMethodObligations :: IO ()
testSourceRejectsAmbiguousDroppedLiteralRangeMethodObligations =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int8) {\nm = \\(x) -> True.\n}.\n"
        <> "impl C(Int16) {\nm = \\(x) -> False.\n}.\n"
        <> "result = (\\(x) -> C::m x) 1."
    )
    "ambiguous qualified method body 'C::m'"

testSourcePreservesLiteralRangeDeferredMethodConstraints :: IO ()
testSourcePreservesLiteralRangeDeferredMethodConstraints =
  assertSourceOkWithoutPrelude
    ( "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int8) {\nm = \\(x) -> True.\n}.\n"
        <> "f = \\(x) -> C::m x.\n"
        <> "result = f 1."
    )

testSourceRejectsAmbiguousLiteralRangeDeferredMethodConstraints :: IO ()
testSourceRejectsAmbiguousLiteralRangeDeferredMethodConstraints =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int8) {\nm = \\(x) -> True.\n}.\n"
        <> "impl C(Int16) {\nm = \\(x) -> False.\n}.\n"
        <> "f = \\(x) -> C::m x.\n"
        <> "result = f 1."
    )
    "ambiguous qualified method body 'C::m'"

testSourceKeepsNestedHelperInferredMethodObligationsScoped :: IO ()
testSourceKeepsNestedHelperInferredMethodObligationsScoped =
  assertSourceOkWithoutPrelude
    ( "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int) {\nm = \\(x) -> True.\n}.\n"
        <> "x = { local = \\(y) -> C::m y. 1. }.\n"
        <> "x."
    )

testSourcePreservesOuterScopeLocalInferredMethodObligations :: IO ()
testSourcePreservesOuterScopeLocalInferredMethodObligations =
  assertSourceOkWithoutPrelude
    ( "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int) {\nm = \\(x) -> True.\n}.\n"
        <> "outer = \\(x) -> { local = C::m x. 1. }.\n"
        <> "result = outer 1."
    )

testSourceResolvesConcreteInferredEqualityObligationsBeforeDroppingThem :: IO ()
testSourceResolvesConcreteInferredEqualityObligationsBeforeDroppingThem =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class Eq(a) { }.\n"
        <> "impl Eq(Int) { }.\n"
        <> "result = (\\(x) -> x == x) True."
    )
    "missing impl fact 'Eq(Bool)'"

testSourceChecksInferredMethodObligationsOnExpressionStatements :: IO ()
testSourceChecksInferredMethodObligationsOnExpressionStatements =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int) { }.\n"
        <> "(\\(x) -> C::m x) 1."
    )
    "missing impl method body 'C::m'"

testSourceChecksInferredEqualityObligationsOnExpressionStatements :: IO ()
testSourceChecksInferredEqualityObligationsOnExpressionStatements =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class Eq(a) { }.\n"
        <> "impl Eq(Int) { }.\n"
        <> "(\\(x) -> x == x) True."
    )
    "missing impl fact 'Eq(Bool)'"

testSourceRejectsAmbiguousInferredEqualityObligationsOnExpressionStatements :: IO ()
testSourceRejectsAmbiguousInferredEqualityObligationsOnExpressionStatements =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class Eq(a) { }.\n"
        <> "\\(x) -> x == x."
    )
    "ambiguous/defaulting inferred constraint 'Eq"

testSourceChecksInferredMethodObligationsOnMonomorphicSignedBindings :: IO ()
testSourceChecksInferredMethodObligationsOnMonomorphicSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class C(a) {\nm :: a -> Bool.\n}.\n"
        <> "impl C(Int) { }.\n"
        <> "result :: Bool.\n"
        <> "result = (\\(x) -> C::m x) 1."
    )
    "missing impl method body 'C::m'"

testSourceRejectsNonTargetQualifiedMethodExactMatch :: IO ()
testSourceRejectsNonTargetQualifiedMethodExactMatch =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class Flag(a) {\nflag :: Int -> Bool.\n}.\n"
        <> "impl Flag(Int) {\nflag = \\(value) -> True.\n}.\n"
        <> "impl Flag(Bool) {\nflag = \\(value) -> False.\n}.\n"
        <> "one :: Int.\n"
        <> "one = 1.\n"
        <> "result = Flag::flag one."
    )
    "ambiguous qualified method body"

testSourceRejectsCallableEqualityBeforeInferredClassObligations :: IO ()
testSourceRejectsCallableEqualityBeforeInferredClassObligations =
  assertSourceSingleErrorContains
    "f = \\(x) -> x.\nbad = f == f."
    "callable values are not equality-supported"

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

testSourceRejectsNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceRejectsNestedEmptyListExactQualifiedMethodSelection =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
        <> "impl RuntimeFlag([[Int]]) {\nflag = \\(values) -> True.\n}.\n"
        <> "impl RuntimeFlag([[Int64]]) {\nflag = \\(values) -> False.\n}.\n"
        <> "(RuntimeFlag::flag) [[1], []]."
    )
    "ambiguous qualified method body 'RuntimeFlag::flag'"

testSourceRejectsConstructorWrappedNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceRejectsConstructorWrappedNestedEmptyListExactQualifiedMethodSelection =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "data Box a = Box a.\n"
        <> "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
        <> "impl RuntimeFlag(Box([[Int]])) {\nflag = \\(box) -> True.\n}.\n"
        <> "impl RuntimeFlag(Box([[Int64]])) {\nflag = \\(box) -> False.\n}.\n"
        <> "(RuntimeFlag::flag) (Box [[1], []])."
    )
    "ambiguous qualified method body 'RuntimeFlag::flag'"

testSourceRejectsOpaqueNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceRejectsOpaqueNestedEmptyListExactQualifiedMethodSelection =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "data Box a = Box a.\n"
        <> "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
        <> "impl RuntimeFlag(Box([[Int]])) {\nflag = \\(box) -> True.\n}.\n"
        <> "impl RuntimeFlag(Box([[Int64]])) {\nflag = \\(box) -> False.\n}.\n"
        <> "make = \\(values) -> Box values.\n"
        <> "(RuntimeFlag::flag) (make [[1], []])."
    )
    "ambiguous qualified method body 'RuntimeFlag::flag'"

testSourceRejectsOpaqueListApplicationExactQualifiedMethodSelection :: IO ()
testSourceRejectsOpaqueListApplicationExactQualifiedMethodSelection =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
        <> "impl RuntimeFlag([[Int]]) {\nflag = \\(values) -> True.\n}.\n"
        <> "impl RuntimeFlag([[Int64]]) {\nflag = \\(values) -> False.\n}.\n"
        <> "make = \\(values) -> values.\n"
        <> "(RuntimeFlag::flag) (make [[1], []])."
    )
    "ambiguous qualified method body 'RuntimeFlag::flag'"

testSourceRejectsBlockProducedNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceRejectsBlockProducedNestedEmptyListExactQualifiedMethodSelection =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
        <> "impl RuntimeFlag([[Int]]) {\nflag = \\(values) -> True.\n}.\n"
        <> "impl RuntimeFlag([[Int64]]) {\nflag = \\(values) -> False.\n}.\n"
        <> "(RuntimeFlag::flag) { values = [[1], []].\nvalues. }."
    )
    "ambiguous qualified method body 'RuntimeFlag::flag'"

testSourceRejectsControlFlowNestedEmptyListExactQualifiedMethodSelection :: IO ()
testSourceRejectsControlFlowNestedEmptyListExactQualifiedMethodSelection =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class RuntimeFlag(a) {\nflag :: a -> Bool.\n}.\n"
        <> "impl RuntimeFlag([[Int]]) {\nflag = \\(values) -> True.\n}.\n"
        <> "impl RuntimeFlag([[Int64]]) {\nflag = \\(values) -> False.\n}.\n"
        <> "(RuntimeFlag::flag) (if True then [[1], []] else [[1], []])."
    )
    "ambiguous qualified method body 'RuntimeFlag::flag'"

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

testSourceSelectsQualifiedFloat64MethodBodyByArgumentTypes :: IO ()
testSourceSelectsQualifiedFloat64MethodBodyByArgumentTypes =
  assertSourceOkWithoutPrelude
    ( "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\n"
        <> "impl Eq(Float64) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"
        <> "left :: Float64.\nleft = 1.5.\n"
        <> "right :: Float64.\nright = 2.25.\n"
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

testSourceRejectsDeferredQualifiedMethodRequirementMissingImplBody :: IO ()
testSourceRejectsDeferredQualifiedMethodRequirementMissingImplBody =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl Eq(Int) { }.\nsame = \\(x) -> Eq::equals x x.\nresult = same 1."
    "missing impl method body 'Eq::equals'"

testSourceRejectsAmbiguousQualifiedMethodBodies :: IO ()
testSourceRejectsAmbiguousQualifiedMethodBodies =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Classify(a) {\nclassify :: Int -> Bool.\n}.\nimpl Classify(Int) {\nclassify = \\(value) -> value == 1.\n}.\nimpl Classify(Bool) {\nclassify = \\(value) -> value == 2.\n}.\nresult = Classify::classify 1.\nresult."
    "ambiguous qualified method body 'Classify::classify' for argument types Int"

testSourceRejectsDuplicateClassDeclarations :: IO ()
testSourceRejectsDuplicateClassDeclarations =
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) { }.\nclass Eq(b) { }.\nx = 1." "E1004"

testSourceRejectsDuplicateConcreteImplDeclarations :: IO ()
testSourceRejectsDuplicateConcreteImplDeclarations = do
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nimpl Eq(Int) { }.\nx = 1." "E1005"
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) { }.\nimpl Eq(Float) { }.\nimpl Eq(Float) { }.\nx = 1." "E1005"
  assertSourceSingleErrorContainsWithoutPrelude "class Eq(a) { }.\nimpl Eq(Float64) { }.\nimpl Eq(Float64) { }.\nx = 1." "E1005"

testSourceRejectsDuplicateAdtImplDeclarations :: IO ()
testSourceRejectsDuplicateAdtImplDeclarations = do
  assertSourceSingleErrorContainsWithoutPrelude "data Color = Red.\nclass Eq(a) { }.\nimpl Eq(Color) { }.\nimpl Eq(Color) { }.\nx = 1." "E1005"
  assertSourceSingleErrorContainsWithoutPrelude "data Box a = Box a.\nclass Eq(a) { }.\nimpl Eq(Box(Int)) { }.\nimpl Eq(Box(Int)) { }.\nx = 1." "E1005"

testCompilerExposesImportedQualifiedMethodBodies :: IO ()
testCompilerExposesImportedQualifiedMethodBodies = do
  result <- compileExpr defaultWarningSettings importedQualifiedMethodFactsProgram
  assertEqual "imported qualified method compile errors" [] (compileErrors result)

testSourceAcceptsSimpleFunctionSignature :: IO ()
testSourceAcceptsSimpleFunctionSignature =
  assertSourceOk "f :: Int -> Int.\nf = (+ 1)."

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

testSourceInstantiatesEqualityConstrainedSignaturePerUse :: IO ()
testSourceInstantiatesEqualityConstrainedSignaturePerUse =
  assertSourceOkWithoutPrelude "class Eq(a) { }.\nimpl Eq(Int) { }.\nimpl Eq(Bool) { }.\nsame :: @{Eq(a)}: a -> a -> Bool.\nsame = \\(x) -> \\(y) -> x == y.\nintValue = same 1 2.\nboolValue = same True False."

testSourceAcceptsUnconstrainedVariablesBesideExplicitConstraints :: IO ()
testSourceAcceptsUnconstrainedVariablesBesideExplicitConstraints =
  assertSourceOk "choose :: @{Eq(a)}: a -> b -> a.\nchoose = \\(x) -> \\(y) -> x.\nintBool = choose 1 True.\nintInt = choose 2 3."

testSourcePreservesPrimitiveConstraintsOnVariableConstrainedSignatures :: IO ()
testSourcePreservesPrimitiveConstraintsOnVariableConstrainedSignatures =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Num(a) { }.\nimpl Num(Bool) { }.\naddSelf :: @{Num(a)}: a -> a.\naddSelf = \\(x) -> x + x.\nbad = addSelf True."
    "cannot apply function"

testSourceRejectsUndeclaredPrimitiveConstraintsOnSignedBindings :: IO ()
testSourceRejectsUndeclaredPrimitiveConstraintsOnSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    "bad :: a -> a.\nbad = \\(x) -> x + 1."
    "does not declare required primitive constraint"

testSourceRejectsUndeclaredClassConstraintsOnSignedBindings :: IO ()
testSourceRejectsUndeclaredClassConstraintsOnSignedBindings =
  assertSourceSingleErrorContainsWithoutPrelude
    ( "class Show(a) { show :: a -> Bool. }.\n"
        <> "impl Show(Int) { show = \\(x) -> True. }.\n"
        <> "bad :: a -> Bool.\n"
        <> "bad = \\(x) -> Show::show x."
    )
    "does not declare required constraint 'Show"

testSourcePreservesExplicitConstraintsWhenPrimitiveRhsHasNoQuantifiedVariables :: IO ()
testSourcePreservesExplicitConstraintsWhenPrimitiveRhsHasNoQuantifiedVariables =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Num(a) { }.\naddSelf :: @{Num(a)}: a -> a.\naddSelf = \\(x) -> x + x.\ngood = addSelf 1."
    "missing impl fact 'Num(Int)'"

testSourcePreservesExplicitEqImplChecksForStructuralConstraints :: IO ()
testSourcePreservesExplicitEqImplChecksForStructuralConstraints =
  assertSourceSingleErrorContainsWithoutPrelude
    "class Eq(a) { }.\nxs :: @{Eq([Int])}: [Int].\nxs = [1]."
    "missing impl fact 'Eq([Int])'"

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
