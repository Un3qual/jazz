# Task 3 Report: Runtime Scope and Explicit-Result-Hint Hardening

## Status

Implemented, committed, and verified.

## RED

Command used before production changes:

```text
cabal test --project-dir=jazz-next runtime-semantics-spec --test-show-details=direct
```

Observed regressions:

```text
FAIL: large flat binding scopes complete within the runtime budget
  100,000-binding flat scope timed out

FAIL: large nested block alias scopes complete within the runtime budget
  50,000-binding nested block alias scope timed out

FAIL: 100,000 explicit result hints render and apply stack safely
  100,000 explicit result hints timed out while rendering or applying
```

The small mixed-hint order/multiplicity case already passed, establishing the semantic contract before changing the representation. After the shared indexed scope plan and lazy prefix environments were implemented, both scope regressions passed while the 100,000-hint case remained red. That isolated the second production change to explicit-result-hint representation and consumption.

## GREEN

Focused command after all runtime changes:

```text
cabal test --project-dir=jazz-next runtime-semantics-spec --test-show-details=direct
```

Result:

```text
PASS: large flat binding scopes complete within the runtime budget
PASS: large nested block alias scopes complete within the runtime budget
PASS: 100,000 explicit result hints render and apply stack safely
PASS: mixed explicit result hints preserve order and multiplicity
PASS: pure and host evaluators preserve diagnostic parity
All RuntimeSemantics tests passed.
Test suite runtime-semantics-spec: PASS
```

The runtime suite also includes the focused recursion, parity, capability-dispatch, primitive, structural-equality, and host-effect cases required by the task.

Full command, run outside the sandbox so Cabal could write its global build log and return an authoritative status:

```text
cabal test --project-dir=jazz-next all --test-show-details=direct
```

Result: exit 0; all 36 Cabal test suites passed.

Additional checks:

```text
cabal exec --project-dir=jazz-next -- ghc -fno-code -Wall -ijazz-next/src \
  jazz-next/src/JazzNext/Compiler/Runtime/Semantics.hs \
  jazz-next/src/JazzNext/Compiler/Runtime/Primitives.hs \
  jazz-next/src/JazzNext/Compiler/Runtime.hs
git diff --check
```

The extracted `Types`, `Semantics`, and `Primitives` modules compiled without warnings. The broader `-Wall` invocation reported only pre-existing warnings in dependencies and `Runtime`; `git diff --check` exited 0 with no output.

## Implementation

- Added regression coverage for a 100,000-binding flat scope, a 50,000-binding nested block-alias path inside qualified-method selection, a 100,000-hint tail-call render/application path, and mixed repeated result hints.
- Added `JazzNext.Compiler.Runtime.ScopePlan` as the shared AST-only indexed plan for pure and host evaluation: indexed statements, module paths, recursive groups, self-recursive functions, binding names, adjacent signatures, numeric targets, host requirements, and function-shape facts.
- Replaced repeated pure-scope list indexing and environment replay with lazy `IntMap` cells plus once-built `scanl'` prefix environments using lazy map insertion. Applied the same concrete strategy to the nested block-local alias environment.
- Kept host execution distinct: it consumes the same static scope plan but retains deferred host cells, scope IDs, cache state, host-recursive knots, and host forcing.
- Added the cycle-breaking `JazzNext.Compiler.Runtime.Types` leaf for runtime values, cells, environments, host-evaluation state, scope results, and module-evaluation mode.
- Replaced nested singular hint wrappers with an opaque `RuntimeExplicitResultHints` sequence. Raw construction is hidden; smart prepend/reattach operations normalize wrappers; the ordered observer and strict fold preserve every outermost-to-innermost hint.
- Preserved discharge order by folding hints outermost-to-innermost while prepending return obligations, so obligations still discharge innermost-to-outermost. Explicit obligations remain excluded from idempotent deduplication.
- Kept `JazzNext.Compiler.Runtime` as the stable evaluator façade and owner of controls, continuations, callable execution, pure/host evaluation, forcing, and recursive alias selection.
- Extracted value/name/type/pattern/numeric-conversion logic into `JazzNext.Compiler.Runtime.Semantics`.
- Extracted builtin, map/filter, operator, arithmetic, and structural-equality dispatch into `JazzNext.Compiler.Runtime.Primitives`, with the one-way dependency `Primitives -> Semantics`.
- Gave `Primitives` one explicit callable-application callback. Runtime supplies the active `ExceptT Diagnostic (RuntimeHostEvaluationT m)` application function, preserving host effect order and deferred-host cache identity without a typeclass or services framework.

## Commits

- `7b3c527a24959cc2d65771f8eeba6abab1f3fafc` — `test(jazz-next): expose runtime scaling regressions`
- `c12c267a97398ac69675d6a26dc3dce6cce8cdee` — `refactor(jazz-next): index shared runtime scope plans`
- `9b790243436cd11837b482bc5a2f18acea2e7659` — `refactor(jazz-next): flatten explicit result hints`
- `5af157b4ca041824518524f8b115d86b00b7c5ab` — `refactor(jazz-next): separate runtime semantic layers`

## Files Changed

- `jazz-next/jazz-next.cabal`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime/Types.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime/ScopePlan.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime/Semantics.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime/Primitives.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/ScopeTests.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

## Self-review

- Confirmed no files under the read-only `jazz-hs/` or `jazz2/` trees changed.
- Confirmed the new dependency graph is acyclic: `Types` is a leaf, `Semantics` depends on `Types`, `Primitives` depends on `Semantics` and `Types`, and `Runtime` consumes all three.
- Confirmed `ScopePlan` remains AST/catalog-only and does not import runtime-value or execution modules.
- Confirmed the raw flat-hint constructor and sequence constructor are not exported, while the compatibility pattern is match-only.
- Confirmed repeated explicit hints are neither reversed nor deduplicated.
- Confirmed the host callback stays in the active host-evaluation transformer stack rather than re-entering through a disabled/pure host adapter.
- Confirmed the large nested-block test reaches the block-local alias prefix/cell path, not only the main module scope.
- Reviewed the complete diff and found no changes outside `jazz-next/` and this required ignored report.

## Concerns

- Initial sandboxed Cabal commands compiled and ran tests successfully but returned nonzero after completion because `/Users/admin/.cache/cabal/logs/build.log` was not writable. The final full suite was rerun with the required permission and exited 0, so this is an environment-only caveat, not a test or implementation failure.
- No unresolved implementation concerns.

## Review fixes

- Extended `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs` so the mixed explicit-result-hint probe applies `mixedExplicitlyHintedCallable 6` to `7`, asserts rendered output `7`, and verifies the discharged value exactly matches `TypeNumeric NumericUInt8` but not `TypeInt`.
- Added `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs` coverage for a hostful `__kernel_map` callback. A deferred dependency `token!` and the entry map share one `RuntimeHostEvaluationT`; each mapper invocation writes its label before returning `token!`. The test asserts two identical raw stdin successes and the exact trace `[WriteStdoutCall "first", ReadStdinCall, WriteStdoutCall "second"]`.

Verification command:

```text
cabal test --project-dir=jazz-next runtime-semantics-spec --test-show-details=direct
```

Result: exit 0; `All RuntimeSemantics tests passed.` and `Test suite runtime-semantics-spec: PASS`. Cabal also printed its known sandbox warning for `/Users/admin/.cache/cabal/logs/build.log`, after reporting the successful suite result.

Self-review: the changes stay at AST/runtime behavior level, make no production-code changes, avoid constructor-depth assertions, and leave the read-only `jazz-hs/` and `jazz2/` trees untouched.
