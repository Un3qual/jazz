{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
  ( Text,
  )
import Jazz.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    Literal (..),
    Pattern (..),
  )
import Jazz.Compiler.DiagnosticCatalog
  ( diagnosticCodeText,
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    diagnosticCode,
  )
import Jazz.Compiler.Driver
  ( CompileResult,
    compileErrors,
    compileExpr,
    compileSource,
  )
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "AdtPatternType" tests

tests :: [NamedTest]
tests =
  [ ( "source pipeline accepts variable pattern binders with the scrutinee type",
      testSourcePipelineAcceptsVariableBinder
    ),
    ( "source pipeline accepts literal and wildcard case patterns",
      testSourcePipelineAcceptsLiteralAndWildcardPatterns
    ),
    ( "source pipeline rejects fractional literal case patterns",
      testSourcePipelineRejectsFractionalLiteralPatterns
    ),
    ( "source pipeline accepts data constructor values",
      testSourcePipelineAcceptsDataConstructorValues
    ),
    ( "source pipeline accepts data constructor application",
      testSourcePipelineAcceptsDataConstructorApplication
    ),
    ( "source pipeline rejects over-applied nullary constructors",
      testSourcePipelineRejectsOverAppliedNullaryConstructor
    ),
    ( "source pipeline accepts data constructor patterns",
      testSourcePipelineAcceptsDataConstructorPatterns
    ),
    ( "source pipeline types constructor pattern binders as payload types",
      testSourcePipelineTypesConstructorPatternBinders
    ),
    ( "source pipeline rejects constructor patterns for incompatible scrutinees",
      testSourcePipelineRejectsConstructorPatternScrutineeMismatch
    ),
    ( "source pipeline rejects unknown constructor patterns",
      testSourcePipelineRejectsUnknownConstructorPatterns
    ),
    ( "source pipeline rejects constructor pattern arity mismatches",
      testSourcePipelineRejectsConstructorPatternArityMismatch
    ),
    ( "source pipeline skips constructor subpatterns after scrutinee mismatch",
      testSourcePipelineSkipsConstructorSubpatternsAfterScrutineeMismatch
    ),
    ( "source pipeline stops constructor argument checks after payload mismatch",
      testSourcePipelineStopsConstructorArgumentChecksAfterPayloadMismatch
    ),
    ( "source pipeline rolls back constructor payload constraints after payload mismatch",
      testSourcePipelineRollsBackConstructorPayloadConstraintsAfterPayloadMismatch
    ),
    ( "source pipeline rejects constructor arm result mismatches",
      testSourcePipelineRejectsConstructorBranchMismatch
    ),
    ( "source pipeline instantiates generic constructor applications independently",
      testSourcePipelineInstantiatesGenericConstructorApplicationsIndependently
    ),
    ( "source pipeline enforces named generic constructor payload types",
      testSourcePipelineEnforcesNamedGenericConstructorPayloadTypes
    ),
    ( "source pipeline resolves earlier local named generic constructor payload types",
      testSourcePipelineResolvesEarlierLocalNamedGenericConstructorPayloadTypes
    ),
    ( "source pipeline rejects mismatched earlier local named generic constructor payloads",
      testSourcePipelineRejectsMismatchedEarlierLocalNamedGenericConstructorPayloadTypes
    ),
    ( "source pipeline rejects unknown generic constructor payload type names",
      testSourcePipelineRejectsUnknownGenericConstructorPayloadTypeNames
    ),
    ( "source pipeline links repeated generic constructor payload parameters",
      testSourcePipelineLinksRepeatedGenericConstructorPayloadParameters
    ),
    ( "source pipeline types recursive generic constructor fields",
      testSourcePipelineTypesRecursiveGenericConstructorFields
    ),
    ( "source pipeline types mutually recursive constructor fields",
      testSourcePipelineTypesMutuallyRecursiveConstructorFields
    ),
    ( "source pipeline rejects recursive generic constructor field mismatches",
      testSourcePipelineRejectsRecursiveGenericConstructorFieldMismatches
    ),
    ( "source pipeline instantiates generic constructor values independently",
      testSourcePipelineInstantiatesGenericConstructorValuesIndependently
    ),
    ( "source pipeline instantiates ordinary bindings that return generic constructors",
      testSourcePipelineInstantiatesOrdinaryBindingsReturningGenericConstructors
    ),
    ( "source pipeline treats constructor payloads as monomorphic",
      testSourcePipelineTreatsConstructorPayloadsAsMonomorphic
    ),
    ( "source pipeline accepts list patterns",
      testSourcePipelineAcceptsListPatterns
    ),
    ( "source pipeline types list pattern binders as element types",
      testSourcePipelineTypesListPatternBinders
    ),
    ( "source pipeline rejects list patterns for incompatible scrutinees",
      testSourcePipelineRejectsListPatternScrutineeMismatch
    ),
    ( "source pipeline stops list element checks after payload mismatch",
      testSourcePipelineStopsListElementChecksAfterPayloadMismatch
    ),
    ( "source pipeline rolls back list element constraints after payload mismatch",
      testSourcePipelineRollsBackListElementConstraintsAfterPayloadMismatch
    ),
    ( "source pipeline rejects list arm result mismatches",
      testSourcePipelineRejectsListBranchMismatch
    ),
    ( "source pipeline accepts cons-like list patterns",
      testSourcePipelineAcceptsConsLikeListPatterns
    ),
    ( "source pipeline types cons-like head binders as element types",
      testSourcePipelineTypesConsLikeHeadBinders
    ),
    ( "source pipeline types cons-like tail binders as list types",
      testSourcePipelineTypesConsLikeTailBinders
    ),
    ( "source pipeline rejects cons-like list patterns for incompatible scrutinees",
      testSourcePipelineRejectsConsLikeListPatternScrutineeMismatch
    ),
    ( "source pipeline accepts tuple patterns",
      testSourcePipelineAcceptsTuplePatterns
    ),
    ( "source pipeline types tuple pattern binders as element types",
      testSourcePipelineTypesTuplePatternBinders
    ),
    ( "source pipeline accepts as-pattern binders with the scrutinee type",
      testSourcePipelineAcceptsAsPatternBinders
    ),
    ( "source pipeline accepts pattern guard binders",
      testSourcePipelineAcceptsPatternGuardBinders
    ),
    ( "source pipeline rejects non-Bool pattern guards",
      testSourcePipelineRejectsNonBoolPatternGuards
    ),
    ( "source pipeline rejects duplicate as-pattern binders",
      testSourcePipelineRejectsDuplicateAsPatternBinders
    ),
    ( "source pipeline accepts or-patterns with common binders",
      testSourcePipelineAcceptsOrPatternCommonBinders
    ),
    ( "source pipeline accepts lambda or-pattern parameters with common binders",
      testSourcePipelineAcceptsLambdaOrPatternCommonBinders
    ),
    ( "source pipeline accepts or-pattern guard binders",
      testSourcePipelineAcceptsOrPatternGuardBinders
    ),
    ( "source pipeline rejects or-pattern binder set mismatches",
      testSourcePipelineRejectsOrPatternBinderSetMismatch
    ),
    ( "source pipeline rejects lambda or-pattern binder set mismatches",
      testSourcePipelineRejectsLambdaOrPatternBinderSetMismatch
    ),
    ( "source pipeline does not expose one-sided or-pattern binders to arm bodies",
      testSourcePipelineDoesNotExposeOneSidedOrPatternBindersToArmBodies
    ),
    ( "source pipeline rejects incompatible or-pattern binder types",
      testSourcePipelineRejectsIncompatibleOrPatternBinderTypes
    ),
    ( "source pipeline rejects incompatible lambda or-pattern binder types",
      testSourcePipelineRejectsIncompatibleLambdaOrPatternBinderTypes
    ),
    ( "source pipeline rejects duplicate binders inside one or-pattern alternative",
      testSourcePipelineRejectsDuplicateBindersInsideOrPatternAlternative
    ),
    ( "core pipeline rejects duplicate binders inside one or-pattern alternative",
      testCorePipelineRejectsDuplicateBindersInsideOrPatternAlternative
    ),
    ( "core pipeline rejects duplicate outer binder inside an or-pattern",
      testCorePipelineRejectsDuplicateOuterBinderInsideOrPattern
    ),
    ( "source pipeline rejects tuple patterns for incompatible scrutinees",
      testSourcePipelineRejectsTuplePatternScrutineeMismatch
    ),
    ( "source pipeline rejects tuple pattern arity mismatches",
      testSourcePipelineRejectsTuplePatternArityMismatch
    ),
    ( "source pipeline rejects duplicate pattern binders",
      testSourcePipelineRejectsDuplicatePatternBinders
    ),
    ( "source pipeline rolls back duplicate binder pattern constraints",
      testSourcePipelineRollsBackDuplicateBinderPatternConstraints
    ),
    ( "source pipeline rejects incompatible literal pattern types",
      testSourcePipelineRejectsIncompatibleLiteralPattern
    ),
    ( "source pipeline skips invalid pattern arm bodies",
      testSourcePipelineSkipsInvalidPatternArmBodies
    ),
    ( "source pipeline rejects mismatched case arm result types",
      testSourcePipelineRejectsMismatchedArmResultTypes
    )
  ]

testSourcePipelineAcceptsVariableBinder :: IO ()
testSourcePipelineAcceptsVariableBinder = do
  result <- compileSource defaultWarningSettings "x = case 1 { | item -> item + 1 }."
  assertCompiles "variable binder result" result

testSourcePipelineAcceptsLiteralAndWildcardPatterns :: IO ()
testSourcePipelineAcceptsLiteralAndWildcardPatterns = do
  result <- compileSource defaultWarningSettings "x = case 1 { | 0 -> False | _ -> True }."
  assertCompiles "literal wildcard result" result

testSourcePipelineRejectsFractionalLiteralPatterns :: IO ()
testSourcePipelineRejectsFractionalLiteralPatterns = do
  result <- compileSource defaultWarningSettings "x = case 1 { | 1.5 -> True | _ -> False }."
  assertSingleDiagnosticContains
    "fractional literal pattern result"
    "fractional literal patterns"
    (compileErrors result)

testSourcePipelineAcceptsDataConstructorValues :: IO ()
testSourcePipelineAcceptsDataConstructorValues = do
  result <- compileSource defaultWarningSettings "data Maybe = Nothing | Just. x = Nothing."
  assertCompiles "data constructor subject" result

testSourcePipelineAcceptsDataConstructorApplication :: IO ()
testSourcePipelineAcceptsDataConstructorApplication = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. x = Just 1."
  assertCompiles "data constructor application" result

testSourcePipelineRejectsOverAppliedNullaryConstructor :: IO ()
testSourcePipelineRejectsOverAppliedNullaryConstructor = do
  result <- compileSource defaultWarningSettings "data Maybe = Nothing. x = Nothing 1."
  assertSingleDiagnosticCode
    "over-applied nullary constructor code"
    "E2006"
    (compileErrors result)
  assertSingleDiagnosticContains
    "over-applied nullary constructor text"
    "cannot apply function of type Maybe"
    (compileErrors result)

testSourcePipelineAcceptsDataConstructorPatterns :: IO ()
testSourcePipelineAcceptsDataConstructorPatterns = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. subject = Just 1. x = case subject { | Just item -> item + 1 | Nothing -> 0 }."
  assertCompiles "data constructor pattern" result

testSourcePipelineTypesConstructorPatternBinders :: IO ()
testSourcePipelineTypesConstructorPatternBinders = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. subject = Just True. x = case subject { | Just item -> item + 1 | Nothing -> 0 }."
  assertSingleDiagnosticCode
    "constructor pattern binder type error code"
    "E2003"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor pattern binder type error text"
    "cannot apply operator '+' to operands of type Bool and Int"
    (compileErrors result)

testSourcePipelineRejectsConstructorPatternScrutineeMismatch :: IO ()
testSourcePipelineRejectsConstructorPatternScrutineeMismatch = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. subject = 1. x = case subject { | Just item -> item | _ -> 0 }."
  assertSingleDiagnosticCode
    "constructor pattern scrutinee mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor pattern scrutinee mismatch text"
    "does not match scrutinee type Int64"
    (compileErrors result)

testSourcePipelineRejectsUnknownConstructorPatterns :: IO ()
testSourcePipelineRejectsUnknownConstructorPatterns = do
  result <- compileSource defaultWarningSettings "subject = [1]. x = case subject { | Just item -> item + 1 | _ -> 0 }."
  assertSingleDiagnosticCode
    "unknown constructor pattern error code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "unknown constructor pattern error text"
    "unknown constructor case pattern 'Just'"
    (compileErrors result)

testSourcePipelineRejectsConstructorPatternArityMismatch :: IO ()
testSourcePipelineRejectsConstructorPatternArityMismatch = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. subject = Just 1. x = case subject { | Just -> 1 | Nothing -> 0 }."
  assertSingleDiagnosticCode
    "constructor pattern arity mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor pattern arity mismatch text"
    "constructor case pattern 'Just' expects 1 argument(s), found 0"
    (compileErrors result)

testSourcePipelineSkipsConstructorSubpatternsAfterScrutineeMismatch :: IO ()
testSourcePipelineSkipsConstructorSubpatternsAfterScrutineeMismatch = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. subject = 1. x = case subject { | Just True -> 0 | _ -> 0 }. y = Just 1."
  assertSingleDiagnosticCode
    "constructor subpattern skip code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor subpattern skip text"
    "does not match scrutinee type Int64"
    (compileErrors result)

testSourcePipelineStopsConstructorArgumentChecksAfterPayloadMismatch :: IO ()
testSourcePipelineStopsConstructorArgumentChecksAfterPayloadMismatch = do
  result <- compileSource defaultWarningSettings "data Pair a b = Pair a b. seed = Pair 1 []. x = case seed { | Pair True [False] -> 0 | _ -> 0 }. ok = Pair 1 [1]."
  assertSingleDiagnosticCode
    "constructor payload mismatch short-circuit code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor payload mismatch short-circuit text"
    "case pattern of type Bool does not match scrutinee type Int"
    (compileErrors result)

testSourcePipelineRollsBackConstructorPayloadConstraintsAfterPayloadMismatch :: IO ()
testSourcePipelineRollsBackConstructorPayloadConstraintsAfterPayloadMismatch = do
  result <- compileSource defaultWarningSettings "data Pair a b = Pair a b. seed = Pair [] 1. x = case seed { | Pair [False] True -> 0 | _ -> 0 }. ok = Pair [1] 1."
  assertSingleDiagnosticCode
    "constructor payload rollback code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor payload rollback text"
    "case pattern of type Bool does not match scrutinee type Int"
    (compileErrors result)

testSourcePipelineRejectsConstructorBranchMismatch :: IO ()
testSourcePipelineRejectsConstructorBranchMismatch = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. subject = Just 1. x = case subject { | Just item -> 1 | Nothing -> False }."
  assertSingleDiagnosticCode
    "constructor branch mismatch code"
    "E2012"
    (compileErrors result)
  assertSingleDiagnosticContains
    "constructor branch mismatch text"
    "case arms must have matching types"
    (compileErrors result)

testSourcePipelineInstantiatesGenericConstructorApplicationsIndependently :: IO ()
testSourcePipelineInstantiatesGenericConstructorApplicationsIndependently = do
  result <- compileSource defaultWarningSettings "data Box a = Box a. first = Box 1. second = Box True."
  assertCompiles "generic constructor applications" result

testSourcePipelineEnforcesNamedGenericConstructorPayloadTypes :: IO ()
testSourcePipelineEnforcesNamedGenericConstructorPayloadTypes = do
  acceptedResult <- compileSource defaultWarningSettings "data Box a = Box Int. good = Box 1."
  assertCompiles "named generic constructor payload type" acceptedResult
  rejectedResult <- compileSource defaultWarningSettings "data Box a = Box Int. bad = Box True."
  assertSingleDiagnosticCode
    "named generic constructor payload mismatch code"
    "E2006"
    (compileErrors rejectedResult)
  assertSingleDiagnosticContains
    "named generic constructor payload mismatch text"
    "cannot apply function of type Int -> Box"
    (compileErrors rejectedResult)

testSourcePipelineResolvesEarlierLocalNamedGenericConstructorPayloadTypes :: IO ()
testSourcePipelineResolvesEarlierLocalNamedGenericConstructorPayloadTypes = do
  result <-
    compileSource
      defaultWarningSettings
      """
      data Status = Ready.
      data Box a = Box Status.
      subject = Box Ready.
      """
  assertCompiles "earlier local named generic constructor payload type" result

testSourcePipelineRejectsMismatchedEarlierLocalNamedGenericConstructorPayloadTypes :: IO ()
testSourcePipelineRejectsMismatchedEarlierLocalNamedGenericConstructorPayloadTypes = do
  result <-
    compileSource
      defaultWarningSettings
      """
      data Status = Ready.
      data Box a = Box Status.
      subject = Box 1.
      """
  assertSingleDiagnosticCode
    "earlier local named generic constructor payload mismatch code"
    "E2006"
    (compileErrors result)
  assertSingleDiagnosticContains
    "earlier local named generic constructor payload mismatch text"
    "cannot apply function of type Status -> Box"
    (compileErrors result)

testSourcePipelineRejectsUnknownGenericConstructorPayloadTypeNames :: IO ()
testSourcePipelineRejectsUnknownGenericConstructorPayloadTypeNames = do
  result <- compileSource defaultWarningSettings "data Box a = Box Foo. subject = Box 1."
  assertSingleDiagnosticCode
    "unknown generic constructor payload code"
    "E2013"
    (compileErrors result)
  assertSingleDiagnosticContains
    "unknown generic constructor payload text"
    "unknown constructor payload type 'Foo'"
    (compileErrors result)

testSourcePipelineLinksRepeatedGenericConstructorPayloadParameters :: IO ()
testSourcePipelineLinksRepeatedGenericConstructorPayloadParameters = do
  result <- compileSource defaultWarningSettings "data Pair a = Pair a a. bad = Pair 1 True."
  assertSingleDiagnosticCode
    "generic constructor repeated parameter code"
    "E2006"
    (compileErrors result)
  assertSingleDiagnosticContains
    "generic constructor repeated parameter text"
    "cannot apply function of type Int -> Pair"
    (compileErrors result)

testSourcePipelineTypesRecursiveGenericConstructorFields :: IO ()
testSourcePipelineTypesRecursiveGenericConstructorFields = do
  result <-
    compileSource
      defaultWarningSettings
      """
      data Tree a
        = Leaf a
        | Branch Tree(a) Tree(a).
      leftmost :: Tree(Bool) -> Bool.
      leftmost = \\(tree) ->
        case tree {
          | Leaf item -> item
          | Branch left _ -> leftmost left
        }.
      answer = leftmost (Branch (Leaf True) (Leaf False)).
      """
  assertCompiles "recursive generic constructor fields" result

testSourcePipelineTypesMutuallyRecursiveConstructorFields :: IO ()
testSourcePipelineTypesMutuallyRecursiveConstructorFields = do
  result <-
    compileSource
      defaultWarningSettings
      """
      data Expression
        = Literal Int
        | Block [Statement].
      data Statement
        = ExpressionStatement Expression.
      program = Block [ExpressionStatement (Literal 42)].
      """
  assertCompiles "mutually recursive constructor fields" result

testSourcePipelineRejectsRecursiveGenericConstructorFieldMismatches :: IO ()
testSourcePipelineRejectsRecursiveGenericConstructorFieldMismatches = do
  result <-
    compileSource
      defaultWarningSettings
      """
      data Tree a
        = Leaf a
        | Branch Tree(a) Tree(a).
      bad :: Tree(Bool).
      bad = Branch (Leaf 1) (Leaf False).
      """
  assertSingleDiagnosticCode
    "recursive generic constructor field mismatch code"
    "E2006"
    (compileErrors result)
  assertSingleDiagnosticContains
    "recursive generic constructor field mismatch text"
    "cannot apply function"
    (compileErrors result)

testSourcePipelineInstantiatesGenericConstructorValuesIndependently :: IO ()
testSourcePipelineInstantiatesGenericConstructorValuesIndependently = do
  result <- compileSource defaultWarningSettings "data Box a = Box a. makeInt = if True then Box else Box. makeBool = if False then Box else Box. first = makeInt 1. second = makeBool True."
  assertCompiles "generic constructor values" result

testSourcePipelineInstantiatesOrdinaryBindingsReturningGenericConstructors :: IO ()
testSourcePipelineInstantiatesOrdinaryBindingsReturningGenericConstructors = do
  result <- compileSource defaultWarningSettings "data Box a = Box a. make = \\(x) -> Box x. first = make 1. second = make True."
  assertCompiles "ordinary binding returning generic constructor" result

testSourcePipelineTreatsConstructorPayloadsAsMonomorphic :: IO ()
testSourcePipelineTreatsConstructorPayloadsAsMonomorphic = do
  result <- compileSource defaultWarningSettings "data Box = Box Int. first = Box 1. second = Box True."
  assertSingleDiagnosticCode
    "monomorphic constructor payload code"
    "E2006"
    (compileErrors result)
  assertSingleDiagnosticContains
    "monomorphic constructor payload text"
    "cannot apply function of type Int -> Box to argument of type Bool"
    (compileErrors result)

testSourcePipelineAcceptsListPatterns :: IO ()
testSourcePipelineAcceptsListPatterns = do
  result <- compileSource defaultWarningSettings "values = [1]. x = case values { | [head] -> head + 1 | [] -> 0 | _ -> 0 }."
  assertCompiles "list pattern" result

testSourcePipelineTypesListPatternBinders :: IO ()
testSourcePipelineTypesListPatternBinders = do
  result <- compileSource defaultWarningSettings "values = [True]. x = case values { | [head] -> head + 1 | _ -> 0 }."
  assertSingleDiagnosticCode
    "list pattern binder type error code"
    "E2003"
    (compileErrors result)
  assertSingleDiagnosticContains
    "list pattern binder type error text"
    "cannot apply operator '+' to operands of type Bool and Int"
    (compileErrors result)

testSourcePipelineAcceptsAsPatternBinders :: IO ()
testSourcePipelineAcceptsAsPatternBinders = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. subject = Just 41. x = case subject { | whole @ Just item -> case whole { | Just nested -> item + nested | Nothing -> 0 } | Nothing -> 0 }."
  assertCompiles "as-pattern binder types" result

testSourcePipelineAcceptsPatternGuardBinders :: IO ()
testSourcePipelineAcceptsPatternGuardBinders = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. subject = Just 1. x = case subject { | Just item if item > 0 -> item | _ -> 0 }."
  assertCompiles "pattern guard binder result" result

testSourcePipelineRejectsNonBoolPatternGuards :: IO ()
testSourcePipelineRejectsNonBoolPatternGuards = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. subject = Just 1. x = case subject { | Just item if item -> item | _ -> 0 }."
  assertSingleDiagnosticContains
    "non-Bool pattern guard text"
    "case guard must have type Bool"
    (compileErrors result)

testSourcePipelineRejectsDuplicateAsPatternBinders :: IO ()
testSourcePipelineRejectsDuplicateAsPatternBinders = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. subject = Just 1. x = case subject { | item @ Just item -> item | Nothing -> 0 }."
  assertSingleDiagnosticCode
    "duplicate as-pattern binder code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "duplicate as-pattern binder text"
    "duplicate case pattern binder 'item'"
    (compileErrors result)

testSourcePipelineAcceptsOrPatternCommonBinders :: IO ()
testSourcePipelineAcceptsOrPatternCommonBinders = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a | Also a. subject = Also 41. x = case subject { | Just item | Also item -> item + 1 | Nothing -> 0 }."
  assertCompiles "or-pattern common binder result" result

testSourcePipelineAcceptsLambdaOrPatternCommonBinders :: IO ()
testSourcePipelineAcceptsLambdaOrPatternCommonBinders = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a | Also a. choose = \\|(Just item | Also item) -> item + 1 |(_) -> 0. x = choose (Also 41)."
  assertCompiles "lambda or-pattern common binder result" result

testSourcePipelineAcceptsOrPatternGuardBinders :: IO ()
testSourcePipelineAcceptsOrPatternGuardBinders = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a | Also a. subject = Just 4. x = case subject { | Just item | Also item if item > 0 -> item | Nothing -> 0 | _ -> 0 }."
  assertCompiles "or-pattern guard binder result" result

testSourcePipelineRejectsOrPatternBinderSetMismatch :: IO ()
testSourcePipelineRejectsOrPatternBinderSetMismatch = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. subject = Nothing. x = case subject { | Just item | Nothing -> 0 | _ -> 1 }."
  assertSingleDiagnosticCode
    "or-pattern binder mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "or-pattern binder mismatch text"
    "or-pattern alternatives must bind the same names"
    (compileErrors result)

testSourcePipelineRejectsLambdaOrPatternBinderSetMismatch :: IO ()
testSourcePipelineRejectsLambdaOrPatternBinderSetMismatch = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. choose = \\(Just item | Nothing) -> 0. x = choose Nothing."
  assertSingleDiagnosticCode
    "lambda or-pattern binder mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "lambda or-pattern binder mismatch text"
    "or-pattern alternatives must bind the same names"
    (compileErrors result)

testSourcePipelineDoesNotExposeOneSidedOrPatternBindersToArmBodies :: IO ()
testSourcePipelineDoesNotExposeOneSidedOrPatternBindersToArmBodies = do
  result <- compileSource defaultWarningSettings "data Maybe a = Nothing | Just a. subject = Nothing. x = case subject { | Just item | Nothing -> item | _ -> 0 }."
  assertContainsDiagnosticCode
    "one-sided or-pattern binder body scope"
    "E1001"
    (compileErrors result)
  assertSingleDiagnosticContains
    "one-sided or-pattern binder body scope text"
    "unbound variable 'item'"
    (filter ((== "E1001") . diagnosticCodeText . diagnosticCode) (compileErrors result))

testSourcePipelineRejectsIncompatibleOrPatternBinderTypes :: IO ()
testSourcePipelineRejectsIncompatibleOrPatternBinderTypes = do
  result <- compileSource defaultWarningSettings "pair = (True, 0). x = case pair { | (item, 0) | (True, item) -> item | _ -> 0 }."
  assertSingleDiagnosticCode
    "or-pattern binder type mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "or-pattern binder type mismatch text"
    "or-pattern binder 'item' has incompatible types"
    (compileErrors result)

testSourcePipelineRejectsIncompatibleLambdaOrPatternBinderTypes :: IO ()
testSourcePipelineRejectsIncompatibleLambdaOrPatternBinderTypes = do
  result <- compileSource defaultWarningSettings "pair = (True, 0). choose = \\((item, 0) | (True, item)) -> item. x = choose pair."
  assertSingleDiagnosticCode
    "lambda or-pattern binder type mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "lambda or-pattern binder type mismatch text"
    "or-pattern binder 'item' has incompatible types"
    (compileErrors result)

testSourcePipelineRejectsDuplicateBindersInsideOrPatternAlternative :: IO ()
testSourcePipelineRejectsDuplicateBindersInsideOrPatternAlternative = do
  result <- compileSource defaultWarningSettings "pair = (1, 2). x = case pair { | (item, item) | (left, right) -> 0 | _ -> 1 }."
  assertSingleDiagnosticCode
    "duplicate binder inside or-pattern code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "duplicate binder inside or-pattern text"
    "duplicate case pattern binder 'item'"
    (compileErrors result)

testCorePipelineRejectsDuplicateBindersInsideOrPatternAlternative :: IO ()
testCorePipelineRejectsDuplicateBindersInsideOrPatternAlternative = do
  result <-
    compileExpr
      defaultWarningSettings
      ( EPatternCase
          (ETuple [ELit (LInt 1), ELit (LInt 2)])
          [ CaseArm
              ( POr
                  [ PTuple [PVariable "item", PVariable "item"],
                    PTuple [PVariable "item", PWildcard]
                  ]
              )
              Nothing
              (EVar "item"),
            CaseArm PWildcard Nothing (ELit (LInt 0))
          ]
      )
  assertSingleDiagnosticCode
    "duplicate binder inside or-pattern alternative code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "duplicate binder inside or-pattern alternative text"
    "duplicate case pattern binder 'item'"
    (compileErrors result)

testCorePipelineRejectsDuplicateOuterBinderInsideOrPattern :: IO ()
testCorePipelineRejectsDuplicateOuterBinderInsideOrPattern = do
  result <-
    compileExpr
      defaultWarningSettings
      ( EPatternCase
          (ETuple [ELit (LInt 1), ELit (LInt 2)])
          [ CaseArm
              ( PAs
                  "item"
                  ( POr
                      [ PTuple [PVariable "item", PWildcard],
                        PTuple [PWildcard, PVariable "item"]
                      ]
                  )
              )
              Nothing
              (EVar "item"),
            CaseArm PWildcard Nothing (ELit (LInt 0))
          ]
      )
  assertSingleDiagnosticCode
    "duplicate outer binder inside or-pattern code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "duplicate outer binder inside or-pattern text"
    "duplicate case pattern binder 'item'"
    (compileErrors result)

testSourcePipelineRejectsListPatternScrutineeMismatch :: IO ()
testSourcePipelineRejectsListPatternScrutineeMismatch = do
  result <- compileSource defaultWarningSettings "subject = 1. x = case subject { | [head] -> head | _ -> 0 }."
  assertSingleDiagnosticCode
    "list pattern scrutinee mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "list pattern scrutinee mismatch text"
    "case pattern of list type does not match scrutinee type Int"
    (compileErrors result)

testSourcePipelineStopsListElementChecksAfterPayloadMismatch :: IO ()
testSourcePipelineStopsListElementChecksAfterPayloadMismatch = do
  result <- compileSource defaultWarningSettings "data Box a = Empty | Box a. seed = [Empty]. x = case seed { | [1, Box False] -> 0 | _ -> 0 }. ok = Box 1."
  assertSingleDiagnosticCode
    "list element mismatch short-circuit code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "list element mismatch short-circuit text"
    "case pattern of type Int does not match scrutinee type Box"
    (compileErrors result)

testSourcePipelineRollsBackListElementConstraintsAfterPayloadMismatch :: IO ()
testSourcePipelineRollsBackListElementConstraintsAfterPayloadMismatch = do
  result <- compileSource defaultWarningSettings "data Box a = Empty | Box a. seed = [Empty]. x = case seed { | [Box False, 1] -> 0 | _ -> 0 }. ok = Box 1."
  assertSingleDiagnosticCode
    "list element rollback code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "list element rollback text"
    "case pattern of type Int does not match scrutinee type Box"
    (compileErrors result)

testSourcePipelineRejectsListBranchMismatch :: IO ()
testSourcePipelineRejectsListBranchMismatch = do
  result <- compileSource defaultWarningSettings "values = [1]. x = case values { | [head] -> 1 | [] -> False }."
  assertSingleDiagnosticCode
    "list branch mismatch code"
    "E2012"
    (compileErrors result)
  assertSingleDiagnosticContains
    "list branch mismatch text"
    "case arms must have matching types"
    (compileErrors result)

testSourcePipelineAcceptsConsLikeListPatterns :: IO ()
testSourcePipelineAcceptsConsLikeListPatterns = do
  result <- compileSource defaultWarningSettings "values = [1, 2]. x = case values { | [head | tail] -> head + hd tail | [] -> 0 }."
  assertCompiles "cons-like list pattern" result

testSourcePipelineTypesConsLikeHeadBinders :: IO ()
testSourcePipelineTypesConsLikeHeadBinders = do
  result <- compileSource defaultWarningSettings "values = [True]. x = case values { | [head | tail] -> head + 1 | _ -> 0 }."
  assertSingleDiagnosticCode
    "cons-like head binder type error code"
    "E2003"
    (compileErrors result)
  assertSingleDiagnosticContains
    "cons-like head binder type error text"
    "cannot apply operator '+' to operands of type Bool and Int"
    (compileErrors result)

testSourcePipelineTypesConsLikeTailBinders :: IO ()
testSourcePipelineTypesConsLikeTailBinders = do
  result <- compileSource defaultWarningSettings "values = [1]. x = case values { | [head | tail] -> tail + 1 | _ -> 0 }."
  assertSingleDiagnosticCode
    "cons-like tail binder type error code"
    "E2003"
    (compileErrors result)
  assertSingleDiagnosticContains
    "cons-like tail binder type error text"
    "cannot apply operator '+' to operands of type [Int64] and Int"
    (compileErrors result)

testSourcePipelineRejectsConsLikeListPatternScrutineeMismatch :: IO ()
testSourcePipelineRejectsConsLikeListPatternScrutineeMismatch = do
  result <- compileSource defaultWarningSettings "subject = 1. x = case subject { | [head | tail] -> head | _ -> 0 }."
  assertSingleDiagnosticCode
    "cons-like list pattern scrutinee mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "cons-like list pattern scrutinee mismatch text"
    "case pattern of list type does not match scrutinee type Int"
    (compileErrors result)

testSourcePipelineAcceptsTuplePatterns :: IO ()
testSourcePipelineAcceptsTuplePatterns = do
  result <- compileSource defaultWarningSettings "pair = (1, 2). x = case pair { | (left, right) -> left + right }."
  assertCompiles "tuple pattern" result

testSourcePipelineTypesTuplePatternBinders :: IO ()
testSourcePipelineTypesTuplePatternBinders = do
  result <- compileSource defaultWarningSettings "pair = (True, 1). x = case pair { | (left, right) -> right + left }."
  assertSingleDiagnosticCode
    "tuple pattern binder type error code"
    "E2003"
    (compileErrors result)
  assertSingleDiagnosticContains
    "tuple pattern binder type error text"
    "cannot apply operator '+' to operands of type Int64 and Bool"
    (compileErrors result)

testSourcePipelineRejectsTuplePatternScrutineeMismatch :: IO ()
testSourcePipelineRejectsTuplePatternScrutineeMismatch = do
  result <- compileSource defaultWarningSettings "subject = 1. x = case subject { | (left, right) -> left | _ -> 0 }."
  assertSingleDiagnosticCode
    "tuple pattern scrutinee mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "tuple pattern scrutinee mismatch text"
    "tuple case pattern does not match scrutinee type Int"
    (compileErrors result)

testSourcePipelineRejectsTuplePatternArityMismatch :: IO ()
testSourcePipelineRejectsTuplePatternArityMismatch = do
  result <- compileSource defaultWarningSettings "pair = (1, 2). x = case pair { | (left, right, extra) -> left | _ -> 0 }."
  assertSingleDiagnosticCode
    "tuple pattern arity mismatch code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "tuple pattern arity mismatch text"
    "tuple case pattern expects 3 element(s), found 2"
    (compileErrors result)

testSourcePipelineRejectsDuplicatePatternBinders :: IO ()
testSourcePipelineRejectsDuplicatePatternBinders = do
  result <- compileSource defaultWarningSettings "values = [1, 2]. x = case values { | [item, item] -> item | _ -> 0 }."
  assertSingleDiagnosticCode
    "duplicate pattern binder code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "duplicate pattern binder text"
    "duplicate case pattern binder 'item'"
    (compileErrors result)

testSourcePipelineRollsBackDuplicateBinderPatternConstraints :: IO ()
testSourcePipelineRollsBackDuplicateBinderPatternConstraints = do
  result <- compileSource defaultWarningSettings "data Pair a b = Empty | Pair a b. seed = Empty. x = case seed { | Pair [item] item -> 0 | _ -> 0 }. ok = Pair 1 1."
  assertSingleDiagnosticCode
    "duplicate binder rollback code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "duplicate binder rollback text"
    "duplicate case pattern binder 'item'"
    (compileErrors result)

testSourcePipelineRejectsIncompatibleLiteralPattern :: IO ()
testSourcePipelineRejectsIncompatibleLiteralPattern = do
  result <- compileSource defaultWarningSettings "x = case True { | 0 -> 1 | _ -> 2 }."
  assertSingleDiagnosticCode
    "pattern type error code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "pattern type error text"
    "does not match scrutinee type"
    (compileErrors result)

testSourcePipelineSkipsInvalidPatternArmBodies :: IO ()
testSourcePipelineSkipsInvalidPatternArmBodies = do
  result <- compileSource defaultWarningSettings "x = case True { | 0 -> 1 + False | _ -> 0 }."
  assertSingleDiagnosticCode
    "invalid pattern arm body is skipped code"
    "E2011"
    (compileErrors result)
  assertSingleDiagnosticContains
    "invalid pattern arm body is skipped text"
    "case pattern of type Int does not match scrutinee type Bool"
    (compileErrors result)

testSourcePipelineRejectsMismatchedArmResultTypes :: IO ()
testSourcePipelineRejectsMismatchedArmResultTypes = do
  result <- compileSource defaultWarningSettings "x = case 1 { | 0 -> True | _ -> 2 }."
  assertSingleDiagnosticCode
    "pattern branch mismatch code"
    "E2012"
    (compileErrors result)
  assertSingleDiagnosticContains
    "pattern branch mismatch text"
    "case arms must have matching types"
    (compileErrors result)

assertCompiles :: Text -> CompileResult -> IO ()
assertCompiles label result = do
  assertEqual (label <> " compile errors") [] (compileErrors result)

assertContainsDiagnosticCode :: Text -> Text -> [Diagnostic] -> IO ()
assertContainsDiagnosticCode label expectedCode diagnostics =
  assertEqual
    label
    True
    (any ((== expectedCode) . diagnosticCodeText . diagnosticCode) diagnostics)
