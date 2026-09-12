# Haskell compiler simplification

User-authorized audit and implementation, stacked on `dfd0bde6` (the latest
local `codex/pr158-review-fixes` and remote architecture-remediation head).
Reduce `src/Jazz` through shared operations; preserve language behavior,
diagnostic ordering/locations, scope identity, evaluation and observation.
Existing specifications containing implementation snippets are outside scope.

- [x] Consolidate the parser's repeated precedence and pattern dispatch.
- [x] Share type-inference operator checking and capability candidate handling.
- [x] Consolidate runtime numeric dispatch, constructor matching and guards.
- [x] Replace analyzer tuple rebuilding with named scope updates; share binding
  registration, duplicate-method diagnostics and warning promotion.
- [x] Review the remaining compiler modules for concrete duplication and use
  existing library operations where they remove custom code.
- [x] Run existing focused and broad correctness suites with GHC 9.14.1,
  formatting and HLint; compare unexpected failures against `dfd0bde6`.
- [x] Independently review the combined changes, fix confirmed regressions,
  record net production-code reduction and commit verified batches.

The repository dispatcher has no ready implementation item; this explicit
maintainer request supplies the scope. Hosted feature work remains deferred.

## Verification checkpoints

- Original `dfd0bde6`: expression parser, binding/signature coherence, runtime,
  rebinding-warning and recursion suites passed in an archived source copy.
- Initial simplifications: all 14 focused suites passed, including primitive
  semantics, parser/pattern/operator/source-range and module contract/export
  coverage. GHC warnings are errors; unused imports/shadowing were corrected.
- Independent reviews of parser, runtime, analyzer, inference and module
  filtering found no semantic regressions. A recursion-review observation led
  to retaining the original skip behavior for non-executable declarations.
- Final correctness matrix: 58 suites passed. The remaining
  `jazz-parser-types-declarations-modules-spec` has ten pre-existing failures;
  rerunning the original `dfd0bde6` produces identical test output after
  removing the log destination line. Cabal stopped scheduling after that
  failure, so the 14 skipped suites were run separately and passed.
- After preserving non-executable declaration skipping, recursion, name and
  runtime suites were rerun and passed. The final CLI builds; executable
  examples, formatting, HLint, Cabal metadata and production-only Weeder pass.
- Full `scripts/ci/haskell-quality.sh` passed in the pinned `.#quality` shell:
  HLint, fresh production and complete component builds, both Weeder policies,
  and generated invariants. The known hosted qualification mismatch is the only
  correctness-suite limitation; it is outside this Haskell simplification.

## Implemented findings

| Area | Shared operation or simplification | Net lines removed |
| --- | --- | ---: |
| Analyzer and module boundaries | Named scope state and incremental updates; one binding registration/rebinding policy, duplicate-method scan, warning-promotion helper, capability filter and inference request path; standard stable deduplication | 219 |
| Parser | One precedence-climbing loop for ordinary expressions, case bodies and guards; shared pattern dispatch with explicit head/constructor-argument context | 124 |
| Runtime | Numeric dispatch by operand shape; shared promotion and predicates, Bool validation, guarded-arm matching and constructor-field traversal | 227 |
| Type inference | Shared operand/section checking, alias lookup, candidate preference, scheme construction and existing type traversal operations | 195 |
| Recursive bindings | Shared recursive environment, indexed eager-child traversal and executable-statement summary | 79 |
| **Total in `src/Jazz`** | **17 files; no added dependency or test scaffolding** | **844** |

The existing phase-indexed compiler and interpreter remain. No generic pass
framework was needed. Exact/compatible matching callbacks, parser boundaries,
alias evidence, definition-site capability facts, numeric error precedence,
observation events and lazy recursive-scope construction remain explicit.

Benchmarks and parser scale suites were not executed. The Haskell quality gate
builds those components only to include their references in dead-code analysis.
