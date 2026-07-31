# Task 3 report: DONE

## Outcome

Task 3 is complete in two commits:

- `b98f12c fix: validate forward signed function references`
- `dea4eae feat: elaborate direct-call functions`

The producer now elaborates the approved expression/direct-call profile into
permanent typed core while keeping ordinary inference compatible. The
permanent validator contract accepts only eligible forward signed functions;
later signed scalars and later unsigned functions remain invisible.

## Step 0 validator amendment

The contract test first established the required RED:

- `forward-signed-function-visibility` failed because the later signed
  function was invisible;
- `forward-signed-scalar-invisibility` passed with the exact invisibility
  failure; and
- `forward-unsigned-function-invisibility` passed with the exact invisibility
  failure.

The Haskell and Jazz validators now mirror a narrow source-ordered
predeclaration rule: only an adjacent concrete monomorphic function signature
and leading lambda are eligible, and an already-visible scheme wins. Module
and block validation paths use the same rule. The permanent contract suite
passed before commit `b98f12c`.

## Producer implementation

The five Task 3 producer files now:

- own the exact ordered manifest of 16 accepted and 20 rejected fixtures;
- retain source-ordered provisional signatures, function bindings, terminal
  expressions, variables, lambdas, applications, scalar literals, and builtin
  binary operators from the normal inference traversal;
- preserve explicit signed, unsigned, and floating numeric widths selected by
  concrete signatures;
- elaborate scalar-parameter returns, direct calls, curried calls, acyclic
  forward-call DAGs, nested calls, canonical `$` calls, and function exports;
- derive concrete monomorphic schemes, closure recipes, binder paths,
  statement paths, module interfaces, and export entries;
- reject callable values, wrong arity, captures, recursion, nonmonomorphic
  functions, nonlocal calls, user-defined operators, and unsupported exports
  with exact structured production failures;
- compute local call SCCs to reject self and mutual recursion while preserving
  acyclic forward calls; and
- return success only after validating the finalized permanent `TypedProgram`.

The tests compare all eight new accepted cases against complete explicit
`TypedProgram` values, compare all nine new callable/operator rejections
against complete exact structured statuses, execute each profile twice, check
ordinary-inference equality, and audit exact failure-kind coverage.

## Verification

Required combined command:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    jazz-typed-core-expression-direct-call-spec \
    jazz-typed-core-contract-spec \
    --jobs=1 --test-show-details=failures
```

Result: both suites passed after the final source cleanup.

Regression command:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    binding-signature-coherence-spec \
    --jobs=1 --test-show-details=failures
```

Result: passed.

Repository-wide command:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next all \
    --jobs=1 --test-show-details=failures
```

Result: all suites passed. The subsequent formatting-only cleanup was checked
for semantic equivalence, and the exact required command plus the coherence
regression were rerun against the cleaned source.

## Self-review

- `git diff --check` passed before staging.
- No conflict markers, TODO/FIXME/HACK markers, undefined values, legacy
  fallbacks, or unrelated files remained.
- Formatter-only churn in `Scope.hs` was removed; the final diff contains the
  semantic changes only.
- The staged diff contained exactly the five Task 3 producer/test files.
- The working tree is clean after commit; this ignored report is the only
  post-commit artifact.

## Concerns

None.

## Fix round 1/5: forward visibility ownership

### Reviewer findings

The first review found two blocking ownership defects:

- `TypeInference.hs` scanned raw root AST tails and injected every matching
  signed lambda into the analyzer's imported-value scope, which changed
  ordinary inference as well as typed-core production.
- `Scope.hs` preloaded every signed leading lambda without deriving the narrow
  eligibility contract from normal signature elaboration.

### RED

The focused producer suite failed after adding the ordinary-inference boundary
regression: ordinary inference incorrectly accepted the later signed function
because the raw-AST analyzer injection made it globally visible.

### Fix

- The source-ordered binding-seed pass now derives forward-function facts only
  in `ProduceTypedCoreExpressionDirectCall`, using
  `signaturePayloadToSignatureType`.
- Eligibility requires an adjacent matching signature and leading lambda, no
  generalized variables or explicit constraints, and a concrete supported
  scalar function type.
- A forward function is visible only from another eligible function binding
  that occurs earlier in the same root scope. Nested blocks receive no forward
  map.
- Provisional function statements retain the eligibility fact from inference;
  the analyzer consumes that fact through `analysisForwardFunctions` after
  inference. Ordinary inference receives an empty forward map.
- The previous `tails`/signature/lambda scan and imported-value injection were
  removed.

The fixed producer manifest remains exactly 16 accepted and 20 rejected
fixtures. The permanent validator manifest remains unchanged at 16 valid and
28 invalid cases, with its existing supplemental three-case parity family.

### Regressions

Supplemental source tests outside the fixed producer manifest now cover:

- an unsigned earlier caller, proving ordinary inference does not inherit
  production-only visibility;
- a later polymorphic function;
- a later evidence-constrained function;
- a later signed scalar; and
- a later unsigned lambda.

Each negative case checks the exact `E1001` subject, ordinary/production
inference equality, repeatability, and diagnostic blocking. The accepted
forward direct-call DAG separately proves that only production inference gains
the approved later-function visibility.

### Verification

The post-fix combined gate passed:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    jazz-typed-core-expression-direct-call-spec \
    jazz-typed-core-contract-spec \
    binding-signature-coherence-spec \
    --jobs=1 --test-show-details=failures
```

The fresh repository-wide gate also passed:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next all \
    --jobs=1 --test-show-details=failures
```

`git diff --check` and the conflict/debug-marker scan passed. The committed
diff contains exactly the six scoped active-compiler and producer-test files;
`jazz-hs/`, `jazz2/`, permanent typed-core constructors, mirrored validators,
and coordination ledgers were unchanged.

### Commit

- `94ac7d2 fix: scope typed-core forward visibility`

### Concerns

None.

## Final whole-branch fix round 1/5: total production and unambiguous identities

### Reviewer findings

The approved amendment in `52d7519` exposed four remaining boundary defects:

- the Haskell and Jazz validators made later signed functions visible inside
  nested blocks, rather than limiting that exception to module statements;
- empty and function-only resolved modules reached a partial `last` during
  typed-program construction;
- an unsupported compound parent discarded profile failures retained by its
  children; and
- producer and lowerer lookup tables silently collapsed function rebinding and
  repeated leading-lambda parameter names.

The lowerer also accumulated functions, parameters, failures, call summaries,
operands, instructions, and expression paths with repeated right appends.

### RED

The validator regression constructed a nested block whose earlier signed
function calls a later signed function. The first Haskell run failed with:

```text
expected [TypedInvisibleName ...], got []
```

The producer/lowerer RED family then demonstrated:

- `empty-module` terminated with `Prelude.last: empty list`;
- duplicate parameters returned only the generic
  `LoweredIRInvalidFunctionShape`;
- the new source and arbitrary-typed identity cases had no structured failure
  constructors; and
- `if True then [1] else [2].` returned only the conditional parent failure.

During implementation, the existing oversaturated-call regression caught one
intermediate mistake: collapsing a function with retained body failures out of
the provisional function table changed the terminal failure from exact arity
to non-local call. Retaining both the lambda shape and recursive failure
channel restored the original ordering.

### Fix

- Module validation still predeclares the narrow later signed-function set.
  Nested block validation now passes an empty forward set in both Haskell and
  Jazz.
- Finalization rejects a scope without a terminal scalar expression at the
  module profile path before typed construction. Module result metadata is
  taken from a safely matched terminal typed expression; the partial `last`
  was removed.
- `InferredExpr` again carries ordered relative production failures.
  Production-aware conditional, list, tuple, binary, lambda, and application
  recursion retains failures from the same inference traversal. Unsupported
  retained subtrees finalize in deterministic structural preorder.
- Function tables are built first-wins only for continued failure analysis,
  while every later same-scope function binding emits
  `TypedCoreFunctionRebindingUnsupported`; no rebound program can succeed.
- Producer finalization reports every duplicate or shadowed leading-lambda
  parameter at its exact expression path before typed construction.
- The lowerer rejects duplicate local function identities before lookup-table
  use and reports every duplicate/shadowed leading parameter structurally as
  `LoweredIRUnsupported`. Both arbitrary typed-program families remain valid
  under the permanent typed-core validator, proving these are lowerer profile
  checks rather than invariant failures.
- Reverse accumulators now preserve source order without repeated right
  appends for function/parameter collection, profile failures, calls,
  application operands, emitted instructions, and recursive expression paths.

The fixed producer manifest remains exactly 16 accepted and 20 rejected
fixtures. The permanent typed-core manifest remains exactly 16 valid and 28
invalid fixtures. All new cases are supplemental.

### Regressions

The supplemental twice-run source producer family now covers:

- an empty resolved module;
- a signed-function-only module;
- a clean conditional with two unsupported list branches;
- same-scope signed function rebinding;
- a repeated multi-parameter lambda name; and
- explicit curried parameter shadowing.

The arbitrary-valid typed-program lowerer family covers duplicate local
function identity and repeated leading-lambda parameter identity. The nested
validator program runs twice in Haskell and also participates in the complete
twice-run Haskell/Jazz parity batch.

### Verification

The combined focused gate passed:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    jazz-typed-core-expression-direct-call-spec \
    jazz-typed-core-contract-spec \
    jazz-lowered-ir-contract-spec \
    binding-signature-coherence-spec \
    --jobs=1 --test-show-details=failures
```

The warning-clean development build passed:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal build --project-dir=jazz-next -fdevelopment all --jobs=1
```

The fresh serialized repository-wide matrix passed:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next all \
    --jobs=1 --test-show-details=failures
```

`cabal check` reported no errors or warnings. Execution-queue checks,
documentation checks, `git diff --check`, and the added conflict/debug-marker
scan passed. The documentation checker emitted only its expected notice that
Prettier enforcement was skipped outside the Nix shell. The amended plan,
design, and queue wording required no further changes.

### Commits

- `e463d7a fix: keep forward visibility module scoped`
- `953f9e6 fix: harden typed-core production identities`

### Concerns

None.

## Fix round 2/5: single signature preparation owner

### Reviewer finding

The first ownership fix still normalized root signatures twice in production:
`allocateBindingSeeds` normalized each candidate during a source-wide
eligibility pass but discarded the resulting solver state, then normal
`SSignature` traversal normalized the same payload again. That left forward
eligibility and ordinary pending-signature state with separate semantic
owners.

The approved plan/design/queue amendment in `8e7a736` explicitly authorized
the narrow `Analyzer.hs` boundary from fix round 1 and required one
source-scope preparation owner for binding seeds, pending signatures, and
production-only forward eligibility.

### RED

The binding/signature ownership suite gained a production-aware scope
regression using:

```text
identity :: a -> a.
identity = \(value) -> value.
```

The regression asserts that the retained provisional signature owns
source-order type variable `0` and that the final solver counter records
exactly two allocations: one signature variable and one binding seed.

Before the fix, the test failed with:

```text
source-ordered prepared signature:
expected TFunctionType (TVarType 0) (TVarType 0),
got TFunctionType (TVarType 1) (TVarType 1)
```

That failure proved the eligibility pass had discarded the first elaboration
state and the later traversal had elaborated the signature again after binding
seed allocation.

### Fix

- `ScopePreparation` now owns source-ordered binding seeds, prepared pending
  signatures, production-only forward-function facts, and the retained solver
  state.
- The preparation fold threads the same source-visible module, capability,
  class/impl, and data declaration context required by normal signature
  normalization.
- Each root `SSignature` calls `signaturePayloadToSignatureType` once in
  preparation. The main source traversal consumes the prepared pending record;
  it no longer normalizes the payload again.
- Preparation retains only its solver partition for the main traversal.
  Diagnostics, declaration processing, generalization, runtime hints, module
  interfaces, and binding checks remain owned by the existing source-order
  traversal.
- Forward eligibility is derived from that same prepared pending signature and
  remains production-only, adjacent, leading-lambda, concrete scalar-function,
  unconstrained, and later-target-only.

The fixed 16 accepted / 20 rejected producer manifest and the permanent 16
valid / 28 invalid validator manifest were unchanged. The existing ordinary,
polymorphic, constrained/evidence, signed-scalar, and unsigned-lambda
visibility regressions all remained green.

### Verification

The focused ownership test was observed RED before implementation and GREEN
afterward. The post-fix combined gate passed:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next \
    jazz-typed-core-expression-direct-call-spec \
    jazz-typed-core-contract-spec \
    binding-signature-coherence-spec \
    --jobs=1 --test-show-details=failures
```

The fresh repository-wide gate passed:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c \
  cabal test --project-dir=jazz-next all \
    --jobs=1 --test-show-details=failures
```

`git diff --check`, the conflict/debug-marker scan, the exact target-file
audit, and the legacy-path audit passed. The committed diff contains only
`Scope.hs` and the binding/signature ownership test. The only root
`SSignature` normalization callsite in `Scope.hs` is inside `prepareScope`;
the other callsite is the pre-existing class-method validator.

### Commit

- `c837d1e fix: prepare scope signatures once`

### Concerns

None.

## Final-fix closeout

The detailed final whole-branch fix round 1/5 section above records the RED
evidence, implementation, supplemental regressions, verification, and commits
for this review pass. Its final state is:

- `e463d7a` limits forward signed-function visibility to module statements;
- `953f9e6` makes module production total, retains ordered compound failures,
  rejects ambiguous producer/lowerer identities, and removes recursive
  right-append accumulators from lowering;
- the focused four-suite gate, warning-clean development build, complete
  serialized Cabal matrix, package check, queue/docs checks, whitespace check,
  and marker scan all pass; and
- no concern remains open.
