---
id: JN-TYPE-SOLVER-CONSTRAINED-SIGNATURE-SCHEMES-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on:
  - JN-TYPE-SOLVER-CONTRACT-001
last_verified: 2026-06-27
completed_on: 2026-06-27
plan_section: "Batch 1: Solver-backed constrained signature schemes"
target_paths:
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Promote variable constrained signatures such as `@{Eq(a)}: a -> a` from monomorphic annotation-only bindings to generalized constrained schemes whose instantiated uses require visible concrete class/impl evidence, while preserving concrete constrained signatures, duplicate/arity diagnostics, numeric defaults, runtime dictionaries, and explicit type application as unchanged or out of scope."
---

# Jazz-Next Solver-Backed Constrained Signatures

> Implementation child for the accepted type-solver contract. All compiler
> changes belong in `jazz-next/`; `jazz-hs/` and `jazz2/` remain read-only.

**Goal:** promote explicit variable constrained signatures from monomorphic
annotation-only checking to generalized constrained schemes with deterministic
use-site evidence checks.

**Architecture:** keep signature parsing and concrete constrained signatures
unchanged. Extend the active `TypeInference.hs` scheme path so a matching
adjacent variable constrained signature can publish explicit scheme constraints,
instantiate them alongside quantified variables, and solve those instantiated
constraints against the class/impl facts visible at the use site. Do not infer
new class constraints from operators or method bodies, do not add runtime
dictionaries, and do not change numeric defaulting.

**Tech Stack:** Haskell `jazz-next` type inference, active class/impl fact
metadata already seeded by the analyzer/type pipeline, focused source-pipeline
coverage in `BindingSignatureCoherenceSpec.hs`, and repo-root queue/docs
validation.

---

## Source Verification

The broad blocker `JN-TYPE-GRAMMAR-CLOSURE-PLAN-001` asks for exactly one
remaining verifier-backed solver child and explicitly says not to batch
inferred class constraints, broad defaulting, solver-backed constrained
signatures, runtime dictionaries, explicit type application, or primitive
mixed-width behavior together.

The primary type grammar plan records `JN-TYPE-SOLVER-CONTRACT-001` as accepted
and says variable constrained signatures such as `@{Eq(a)}: a -> a` should
graduate from the current monomorphic annotation-only behavior to generalized
constrained schemes when the solver-backed constrained-signature child lands.
It also says concrete and currently monomorphic constrained-signature behavior
was not reworked by the completed ordinary-binding schemes child.

The active bindings/signatures spec says the current baseline accepts
variable constrained signatures only as monomorphic annotation-only bindings,
while inferred class constraints, broad defaulting, solver-backed constrained
signatures, runtime evidence, and explicit type application remain future
verifier-backed child rows.

The current implementation shape is narrow enough for this child:

- `TypeInference.hs` represents generalized bindings as
  `TypeScheme (Set Int) ExpressionType`, with no scheme context yet.
- `signaturePayloadToExpressionType` already has a dedicated
  `supportedVariableConstraints` branch that allocates one set of signature
  variables and turns the supported surface into a monomorphic type.
- `BindingSignatureCoherenceSpec.hs` already contains the regression
  `testSourceKeepsVariableConstrainedSignatureMonomorphic`, which should flip
  from the current failure expectation to the new generalized constrained
  scheme behavior.

## Batch 1: Solver-backed constrained signature schemes

Executor-safe scope:

- Accept only explicit variable constrained signatures that the current parser
  already represents, for example `id :: @{Eq(a)}: a -> a.`.
- Preserve the existing accepted concrete constrained-signature behavior,
  including visible class/impl fact checks for concrete arguments.
- Preserve duplicate-constraint diagnostics, wrong-arity diagnostics,
  unsupported type-application diagnostics, unused constrained variable
  diagnostics, and signature-primary spans.
- Publish a generalized constrained scheme only for an adjacent binding whose
  matching signature has supported variable constraints.
- Instantiate explicit scheme constraints freshly at every use of that binding.
- Solve each instantiated explicit constraint against the class declaration and
  concrete impl facts visible at the use site once the use expression has
  supplied concrete argument types.
- Report deterministic missing-evidence diagnostics when an instantiated
  explicit constraint resolves to a concrete type without a visible matching
  impl fact.
- Report deterministic ambiguity/defaulting diagnostics when an instantiated
  explicit constraint still contains unresolved type variables at the end of
  the enclosing statement.

Out of scope:

- inferring class constraints from ordinary expressions, operators, or method
  bodies,
- broad defaulting beyond the existing literal defaults,
- runtime dictionaries or runtime evidence values,
- abstraction method dispatch changes,
- explicit type application syntax,
- higher-rank polymorphism,
- generic constructor pattern typing,
- module/import behavior,
- primitive mixed-width or implicit numeric promotion behavior,
- concrete constrained-signature rework,
- any `jazz-hs/` or `jazz2/` edits.

Target paths:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Implementation outline:

1. In `TypeInference.hs`, replace the plain two-field `TypeScheme` with a
   scheme type that can carry explicit constraints, for example quantified
   variables, a list of instantiated signature constraints, and the expression
   type. Keep ordinary inferred bindings and recursive-group schemes using an
   empty constraint list so the completed ordinary-binding child remains
   behaviorally unchanged.
2. Replace `signaturePayloadToExpressionType` with a small result record or
   tuple that carries both the normalized declared type and any explicit
   variable constraint context. Empty constrained signatures and concrete
   constrained signatures should return the same declared type with an empty
   context.
3. Change the variable constrained-signature path so it validates supported
   unary constraints against visible class facts and declared arity, allocates
   signature variables once, converts the body type using those variables, and
   records the corresponding constraint arguments against those same internal
   variables.
4. Extend `PendingSignatureType` so the pending adjacent signature can carry
   the explicit constraint context as well as the declared type and span.
5. In `ordinaryBindingForValue`, keep ordinary adjacent concrete signatures
   monomorphic, but allow a matching pending variable constrained signature to
   publish a `SchemeTypeBinding` whose quantified variables are the eligible
   variables from the declared type and whose explicit constraints reference
   those variables. Direct constructor aliases remain monomorphic.
6. In `instantiateTypeScheme`, instantiate explicit scheme constraints with the
   same fresh variable map used for the scheme type. Add the instantiated
   constraints to inference state as statement-local deferred explicit
   constraints, paired with the class/impl fact snapshot visible at the use
   site.
7. Resolve statement-local deferred explicit constraints after each `SLet` and
   `SExpr` inference has completed its normal unification. A resolved concrete
   constraint must have matching visible class arity and concrete impl fact.
   A still-variable constraint must produce an ambiguity/defaulting diagnostic
   without introducing new defaulting behavior.
8. Add a missing-evidence diagnostic helper that names the instantiated
   constraint and the missing impl fact, for example `missing impl fact
   'Eq(Bool)'`, and reuse the existing enclosing statement-span annotation
   pattern.
9. Keep the current `supportedVariableConstraints` rejection cases green:
   duplicate constraints, unused constrained variables, unconstrained body
   variables, type applications, and function-type constraint arguments should
   still produce `E2009` from the signature statement.

Focused coverage:

- Replace `testSourceKeepsVariableConstrainedSignatureMonomorphic` with an
  acceptance test proving one explicitly constrained scheme can be used at both
  `Int` and `Bool` in the same scope:

  ```jz
  id :: @{Eq(a)}: a -> a.
  id = \(x) -> x.
  x = id 1.
  y = id True.
  ```

- Add a no-prelude acceptance test proving use-site facts are honored without
  relying on bundled prelude facts:

  ```jz
  class Eq(a) { }.
  impl Eq(Int) { }.
  impl Eq(Bool) { }.
  id :: @{Eq(a)}: a -> a.
  id = \(x) -> x.
  x = id 1.
  y = id True.
  ```

- Add a no-prelude negative test proving instantiated explicit constraints
  require visible use-site evidence:

  ```jz
  class Eq(a) { }.
  impl Eq(Int) { }.
  id :: @{Eq(a)}: a -> a.
  id = \(x) -> x.
  ok = id 1.
  bad = id True.
  ```

  Expected: one deterministic diagnostic containing `missing impl fact
  'Eq(Bool)'`.

- Keep the existing concrete constrained-signature tests green, including
  `@{Eq(Int)}: Int`, tuple concrete arguments, ADT application concrete
  arguments, missing concrete impl facts, duplicate constraints, and unsupported
  variable/type-application cases.

- Keep the completed ordinary-binding scheme tests green, especially fresh
  per-use unconstrained variables beside numeric/equality constraints and the
  recursive-group generalization cases.

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```
