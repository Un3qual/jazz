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
- [ ] Run existing focused and broad correctness suites with GHC 9.14.1,
  formatting and HLint; compare unexpected failures against `dfd0bde6`.
- [ ] Independently review the combined changes, fix confirmed regressions,
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
- The final broad correctness matrix is running.
