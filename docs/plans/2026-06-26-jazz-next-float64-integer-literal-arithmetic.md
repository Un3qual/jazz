---
id: JN-PRIMITIVE-FLOAT64-INTEGER-LITERAL-ARITHMETIC-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-06-26
plan_section: "Task 1: Float64 integer-literal arithmetic targeting"
target_paths:
  - docs/spec/runtime/primitive-semantics.md
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Accept direct binary `+`, `-`, `*`, and `/` when exactly one operand is an uncommitted integer literal and the other operand resolves to `Float`/`Float64`, producing Float64 arithmetic while preserving typed Int value rejection, Float16/Float32 rejection, comparison/equality rejection, mixed-width rejection, and no solver/operator-value/section behavior."
---

# Float64 Integer-Literal Arithmetic Targeting Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Accept the smallest post-suffix primitive numeric delta: direct
binary Float64 arithmetic may contextually target one uncommitted integer
literal operand as Float64.

**Architecture:** Keep the existing explicit-conversion and same-concrete-width
model as the default. Add one local type-inference rule for direct arithmetic
binary expressions where one side is an integer literal and the other side is
already in the `Float`/`Float64` domain, then add the matching runtime fallback
for untyped integer values paired with default or targeted Float64 values. Do
not change global unification, operator values, sections, comparison/equality,
Float16/Float32, or solver/defaulting behavior.

**Tech Stack:** Haskell `jazz-next` type inference/runtime, active primitive
semantics spec, focused `PrimitiveSemanticsSpec` and `RuntimeSemanticsSpec`
`runghc` verification.

---

## Source Verification

The primitive umbrella is blocked after the landed fractional literal suffix
child until a separate contract accepts a concrete post-suffix primitive delta.
The blocker contract names implicit integer-to-float promotion as one possible
delta, but explicitly excludes batching it with mixed-width behavior, broader
solver/defaulting, callable identity, or user-defined operator behavior.

The active primitive spec currently says explicit numeric conversions are the
only numeric conversions and that suffix work did not add implicit
integer-to-float promotion. This child is the required separate narrow contract:
only uncommitted integer literals in direct binary Float64 arithmetic are
contextually targeted as Float64.

The active tests already identify the exact rejection points this child should
split: `PrimitiveSemanticsSpec` rejects `x = 1 + 1.5.` as mixed
integer/fractional arithmetic, and it separately rejects implicit
integer-to-`Float16`/`Float32` arithmetic, comparison, and equality. The child
turns only the Float64 arithmetic literal case into acceptance and preserves
the other rejections.

## Task 1: Float64 integer-literal arithmetic targeting

Scope:

- Accept direct binary `+`, `-`, `*`, and `/` when one operand is an
  uncommitted integer literal and the other operand resolves to `Float` or
  `Float64`.
- Return the peer Float64-domain type: `Float` stays `Float`, explicit
  `Float64` stays `Float64`.
- Preserve existing all-integer arithmetic for two integer literals.
- Preserve existing same concrete `Float`, `Float16`, `Float32`, and `Float64`
  arithmetic when both operands are already floats of the same concrete width.
- Preserve rejection for typed integral values mixed with `Float`/`Float64`,
  including `Int`, `Int64`, and width-specific integer signatures.
- Preserve rejection for `Float16`/`Float32` mixed with integer literals,
  default `Float`, or any other concrete width.
- Preserve rejection for integer-literal `Float64` comparison and equality:
  `1 < 1.5`, `1 == 1.0`, and `toFloat64 1 == 1` stay errors.
- Preserve rejection for operator values and sections in this child:
  `(+) 1 1.5`, `(1 +) 1.5`, and `(+ 1.5) 1` stay out of scope.
- Do not add implicit promotion for typed `Int` variables, mixed concrete
  float widths, broad numeric solver/defaulting, typeclass dispatch,
  callable identity, or user-defined operator behavior.

Target paths:

- `docs/spec/runtime/primitive-semantics.md`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Deliverable:

- Direct source expressions such as `1 + 1.5`, `1.5 + 2`, `toFloat64 1 + 2`,
  and `6 / 2.0` compile and evaluate as Float64-domain arithmetic.
- Typed integral values such as `left :: Int64. left = 1. left + 1.5` still
  fail at compile time.
- `Float16`/`Float32`, comparison/equality, operator values, sections,
  user-defined operators, and solver/defaulting behavior remain unchanged.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

Implementation steps:

- [ ] **Step 1: Add source-pipeline acceptance coverage**

  In `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`,
  add this test near the existing Float64 arithmetic and mixed-width
  rejection tests:

  ```haskell
  testSourcePipelineAcceptsFloat64IntegerLiteralArithmetic :: IO ()
  testSourcePipelineAcceptsFloat64IntegerLiteralArithmetic = do
    assertCompiles
      "sumLeft = 1 + 1.5.\nsumRight = 1.5 + 2.\ndiff = 5 - 2.5.\nprod = 2 * 1.5.\nquotient = 6 / 2.0."
    assertCompilesWithBundledPrelude
      "left = toFloat64 1.\nright = toFloat64 2.\nfromLeft = left + 2.\nfromRight = 3 + right."
  ```

  Add the test label to the suite's top-level list:

  ```haskell
  ("source pipeline accepts Float64 integer-literal arithmetic", testSourcePipelineAcceptsFloat64IntegerLiteralArithmetic),
  ```

- [ ] **Step 2: Preserve source-pipeline rejection coverage**

  Replace the old `testSourcePipelineRejectsImplicitIntegerFractionalMixing`
  case with a typed-integral rejection test, and keep the existing
  `Float16`/`Float32`, comparison/equality, operator-value, and section
  rejection tests unchanged or add them if missing:

  ```haskell
  testSourcePipelineRejectsTypedIntegerFloat64Arithmetic :: IO ()
  testSourcePipelineRejectsTypedIntegerFloat64Arithmetic = do
    assertCompileError
      "left :: Int64.\nleft = 1.\nx = left + 1.5."
      "typed Int64/Float64 arithmetic"
      "E2003"
    assertCompileError
      "left :: Int.\nleft = 1.\nx = 1.5 + left."
      "typed Int/Float64 arithmetic"
      "E2003"
  ```

  Add focused out-of-scope assertions if the suite does not already cover
  them:

  ```haskell
  testSourcePipelineRejectsFloat64IntegerLiteralComparisonEquality :: IO ()
  testSourcePipelineRejectsFloat64IntegerLiteralComparisonEquality = do
    assertCompileError
      "x = 1 < 1.5."
      "integer literal Float64 comparison"
      "E2003"
    assertCompileError
      "x = 1 == 1.0."
      "integer literal Float64 equality"
      "E2004"

  testSourcePipelineRejectsFloat64IntegerLiteralOperatorValuesSections :: IO ()
  testSourcePipelineRejectsFloat64IntegerLiteralOperatorValuesSections = do
    assertCompileError
      "x = (+) 1 1.5."
      "integer literal Float64 operator value"
      "E2003"
    assertCompileError
      "x = (1 +) 1.5."
      "integer literal Float64 left section"
      "E2003"
    assertCompileError
      "x = (+ 1.5) 1."
      "integer literal Float64 right section"
      "E2003"
  ```

- [ ] **Step 3: Keep the type-inference change local to direct arithmetic**

  In `jazz-next/src/JazzNext/Compiler/TypeInference.hs`, update
  `applyNumericBinaryRule` so it tries the new direct-arithmetic rule before
  the existing `unifyTypes` path:

  ```haskell
  applyNumericBinaryRule operatorSymbol resultRule leftType rightType state =
    case integerLiteralFloat64ArithmeticOperand resultRule state leftType rightType of
      Just (resolvedOperandType, stateAfterNumericConstraint) ->
        (Just (numericRuleResultType resultRule resolvedOperandType), stateAfterNumericConstraint)
      Nothing ->
        case unifyTypes leftType rightType state of
          Just stateAfterUnify ->
            let resolvedOperandType = numericBinaryOperandType operatorSymbol resultRule stateAfterUnify leftType rightType
             in case constrainNumericOperatorType (numericRuleConstraint resultRule) resolvedOperandType stateAfterUnify of
                  Just stateAfterNumericConstraint ->
                    (Just (numericRuleResultType resultRule resolvedOperandType), stateAfterNumericConstraint)
                  Nothing ->
                    numericOperandError stateAfterUnify
          Nothing -> numericOperandError state
    where
      numericOperandError errState =
        ( Nothing,
          addTypeError
            errState
            ( mkNumericBinaryTypeError
                operatorSymbol
                resultRule
                (resolveType errState leftType)
                (resolveType errState rightType)
            )
        )
  ```

  Add helpers near `numericBinaryOperandType`:

  ```haskell
  integerLiteralFloat64ArithmeticOperand ::
    NumericRuleResult ->
    InferState ->
    ExpressionType ->
    ExpressionType ->
    Maybe (ExpressionType, InferState)
  integerLiteralFloat64ArithmeticOperand resultRule state leftType rightType =
    case resultRule of
      NumericBoolResult -> Nothing
      NumericSameTypeResult ->
        case (resolveType state leftType, resolveType state rightType) of
          (TIntegerLiteralType literalRange, floatType)
            | integerLiteralRangeFitsFloat64 literalRange,
              expressionTypeIsFloat64Domain floatType ->
                arithmeticResult floatType
          (floatType, TIntegerLiteralType literalRange)
            | expressionTypeIsFloat64Domain floatType,
              integerLiteralRangeFitsFloat64 literalRange ->
                arithmeticResult floatType
          _ -> Nothing
    where
      arithmeticResult floatType = do
        stateAfterConstraint <-
          constrainNumericOperatorType RuntimeArithmeticNumericConstraint floatType state
        Just (floatType, stateAfterConstraint)

  expressionTypeIsFloat64Domain :: ExpressionType -> Bool
  expressionTypeIsFloat64Domain expressionType =
    case expressionType of
      TFloatType -> True
      TNumericType NumericFloat64 -> True
      _ -> False

  integerLiteralRangeFitsFloat64 :: IntegerLiteralRange -> Bool
  integerLiteralRangeFitsFloat64 literalRange =
    case numericTypeFloatIntegerBounds NumericFloat64 of
      Just (lowerBound, upperBound) ->
        let (literalMin, literalMax) = integerLiteralRangeBounds literalRange
         in literalMin >= lowerBound && literalMax <= upperBound
      Nothing -> False
  ```

  Do not change `unifyTypes`, `integerLiteralRangeFitsNumericType`,
  `numericSectionCounterpartType`, or `instantiateOperatorType` in this child.

- [ ] **Step 4: Add runtime evaluation for the matching fallback only**

  In `jazz-next/src/JazzNext/Compiler/Runtime.hs`, add direct `VInt`/`VFloat`
  arithmetic cases after the same-family integer and float arithmetic cases.
  The helper should accept only untyped integer metadata and default or
  targeted Float64 metadata:

  ```haskell
  ("+", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
    | runtimeIntTargetType leftMetadata == Nothing,
      runtimeFloatMetadataIsFloat64Domain rightMetadata ->
        evalFloatArithmetic "+" (float64PeerMetadata rightMetadata) rightMetadata (fromInteger leftInt + rightFloat)
  ("+", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
    | runtimeFloatMetadataIsFloat64Domain leftMetadata,
      runtimeIntTargetType rightMetadata == Nothing ->
        evalFloatArithmetic "+" leftMetadata (float64PeerMetadata leftMetadata) (leftFloat + fromInteger rightInt)
  ```

  Repeat the same shape for `-` and `*`. For `/`, keep division-by-zero
  diagnostics deterministic:

  ```haskell
  ("/", VFloat _ leftMetadata, VInt 0 rightMetadata)
    | runtimeFloatMetadataIsFloat64Domain leftMetadata,
      runtimeIntTargetType rightMetadata == Nothing ->
        Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
  ("/", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
    | runtimeIntTargetType leftMetadata == Nothing,
      runtimeFloatMetadataIsFloat64Domain rightMetadata,
      floatIsZero rightFloat ->
        Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
  ("/", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
    | runtimeIntTargetType leftMetadata == Nothing,
      runtimeFloatMetadataIsFloat64Domain rightMetadata ->
        evalFloatArithmetic "/" (float64PeerMetadata rightMetadata) rightMetadata (fromInteger leftInt / rightFloat)
  ("/", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
    | runtimeFloatMetadataIsFloat64Domain leftMetadata,
      runtimeIntTargetType rightMetadata == Nothing ->
        evalFloatArithmetic "/" leftMetadata (float64PeerMetadata leftMetadata) (leftFloat / fromInteger rightInt)
  ```

  Add helpers near the existing float target helpers:

  ```haskell
  runtimeFloatMetadataIsFloat64Domain :: RuntimeFloatMetadata -> Bool
  runtimeFloatMetadataIsFloat64Domain metadata =
    case runtimeFloatTargetType metadata of
      Just NumericFloat64 -> True
      Nothing -> True
      _ -> False

  float64PeerMetadata :: RuntimeFloatMetadata -> RuntimeFloatMetadata
  float64PeerMetadata metadata =
    case runtimeFloatTargetType metadata of
      Just NumericFloat64 -> targetedFloatMetadata NumericFloat64
      Nothing -> untypedFloatMetadata Nothing
      _ -> metadata
  ```

  Do not add `VInt`/`VFloat` comparison or equality cases.

- [ ] **Step 5: Add runtime success and fallback rejection coverage**

  In `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`,
  add source-level runtime coverage near the existing Float64 arithmetic tests:

  ```haskell
  testFloat64IntegerLiteralArithmeticRuntimeSuccess :: IO ()
  testFloat64IntegerLiteralArithmeticRuntimeSuccess = do
    result <-
      runSource
        defaultWarningSettings
        "sumLeft = 1 + 1.5.\nsumRight = 1.5 + 2.\ndiff = 5 - 2.5.\nprod = 2 * 1.5.\nquotient = 1 / 2.0.\n(sumLeft, sumRight, diff, prod, quotient)."
    assertEqual "compile errors" [] (runCompileErrors result)
    assertEqual "runtime errors" [] (runRuntimeErrors result)
    assertEqual "runtime output" (Just "(2.5, 3.5, 2.5, 3.0, 0.5)") (runOutput result)
  ```

  Add direct runtime fallback checks to prove the helper stays narrow:

  ```haskell
  testRuntimeFallbackRejectsTypedIntegerFloat64Arithmetic :: IO ()
  testRuntimeFallbackRejectsTypedIntegerFloat64Arithmetic =
    assertRuntimeErrorContains
      "runtime fallback typed Int64 plus Float64"
      "E3007"
      (evaluateRuntimeExpr (runtimeExpr (EBinary "+" (targetedInt "__kernel_toInt64") (targetedFloat "__kernel_toFloat64"))))

  testRuntimeFallbackRejectsIntegerLiteralNarrowFloatArithmetic :: IO ()
  testRuntimeFallbackRejectsIntegerLiteralNarrowFloatArithmetic =
    assertRuntimeErrorContains
      "runtime fallback untyped Int plus Float16"
      "E3007"
      (evaluateRuntimeExpr (runtimeExpr (EBinary "+" (ELit (LInt 1)) (targetedFloat "__kernel_toFloat16"))))
  ```

  Add the success and rejection labels to the top-level suite list.

- [ ] **Step 6: Update the active primitive spec for this narrow exception**

  In `docs/spec/runtime/primitive-semantics.md`, update the width/defaulting
  contract with this exact narrow exception:

  ```markdown
  - Direct binary `+`, `-`, `*`, and `/` may contextually target exactly one
    uncommitted integer literal operand as `Float`/`Float64` when the other
    operand already resolves to the `Float`/`Float64` domain. This is literal
    targeting, not typed `Int` promotion: typed integral values, operator
    values, sections, comparison/equality, `Float16`, `Float32`, mixed concrete
    widths, and broader solver/defaulting behavior still require explicit
    conversions or remain rejected.
  ```

  Keep the explicit conversion contract otherwise intact by changing only the
  blanket sentence `There are no implicit numeric conversions.` to:

  ```markdown
  There are no implicit numeric conversions except the direct binary
  Float64-domain integer-literal arithmetic targeting rule above.
  ```

- [ ] **Step 7: Run focused verification**

  Run:

  ```bash
  bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  ```

  Expected result: all commands pass. If docs validation fails only because
  central queue integration has not happened yet, keep the exact failure text
  with the implementation handoff.

## Self-Review Notes

- Spec coverage: this plan covers exactly one post-suffix primitive delta and
  names the implementation, docs, and focused primitive test paths.
- Placeholder scan: no `TBD`, broad `TODO`, or unbounded "handle edge cases"
  steps are intentionally left.
- Scope boundary: this child intentionally does not touch `jazz-hs/` or
  `jazz2/`, does not edit `docs/execution/queue.md`, and does not reopen
  mixed-width arithmetic/comparison, broader numeric solver/defaulting,
  callable identity, or user-defined operator behavior.
