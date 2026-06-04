# Blocker Unblocker Contracts

This file turns `docs/execution/queue.md` blocked rows into promotion-ready
handoffs. Use it before opening old plan history.

When `Ready Now` is empty:

1. Read `docs/execution/queue.md`.
2. Use the ordered `Next Curation Target` candidates in that file.
3. Open only the matching section below and the named source plan/spec.
4. Promote exactly one child by creating or updating a child plan with concrete
   frontmatter, then add the matching `Ready Now` row.

A blocked row should not send an executor on a broad docs scan. If the contract
below is stale, refresh this file and the queue row instead of searching
unrelated plans.

## Promotion Contract Template

Each blocked item should answer these questions:

- `Smallest unblocker`: the next action that can make progress.
- `Decision needed`: the exact missing product, language, or runtime decision.
- `Recommended default`: the default to take if no stronger source contradicts
  it.
- `Candidate child`: the child row or child plan to create next.
- `Target paths`: concrete files for that child, with non-doc paths for
  implementation candidates.
- `Verification`: exact commands to close the child.
- `Not in scope`: the work that must not be smuggled into the child.

## Current Blockers

### JN-ABSTRACTION-SEMANTICS-PLAN-001

- Smallest unblocker: split one bundled-prelude method-body child from the broad
  abstraction umbrella.
- Decision needed: none for the next child if it is limited to `Eq(Bool).equals`.
- Recommended default: mirror the landed `Eq(Int).equals` path for Bool only;
  do not introduce dictionary values, default methods, superclasses, inferred
  constraints, solver behavior, or module method export/import semantics.
- Candidate child: `JN-ABSTRACTION-PRELUDE-EQ-BOOL-METHOD-001`.
- Target paths: `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`,
  `jazz-next/stdlib/Prelude.jz`,
  `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`,
  `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`,
  `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`.
- Verification:
  `bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`;
  `bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`;
  `bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`;
  `bash scripts/check-execution-queue.sh`; `bash scripts/check-docs.sh`.
- Not in scope: unqualified overloads, dictionary passing, runtime evidence
  values, default methods, superclasses, overlap/orphan policy beyond duplicate
  visible facts, inferred constraints, or method import/export rules.

### JN-USER-DEFINED-OPERATORS-PLAN-001

- Smallest unblocker: lock a Stage 2 fixed-tier user-defined operator contract
  before parser work.
- Decision needed: exact declaration syntax, allowed symbol characters, and how
  declarations choose from the frozen precedence tiers.
- Recommended default: require explicit operator declarations, restrict them to
  a fixed allowed symbol set, and choose from existing tiers instead of adding
  custom precedence in the first child.
- Candidate child: `JN-OPERATORS-STAGE2-FIXED-TIER-CONTRACT-001`.
- Target paths: `docs/spec/syntax/operators.md`,
  `docs/plans/spec-clarification/2026-03-03/syntax/15-operator-fixity-and-sections.md`,
  `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: custom precedence declarations, new builtin operators, runtime
  overload dispatch, or parser support before the contract lands.

### JN-PRIMITIVE-SURFACE-EXPANSION-PLAN-001

- Smallest unblocker: choose the next primitive delta contract, starting with
  literal suffixes rather than implicit promotion or mixed-width arithmetic.
- Decision needed: suffix spelling, accepted width families, and whether suffixes
  are parser syntax or ordinary prelude APIs.
- Recommended default: specify literal suffix syntax first and keep implicit
  integer-to-float promotion, implicit mixed-width behavior, and broader numeric
  solver work blocked.
- Candidate child: `JN-PRIMITIVE-LITERAL-SUFFIX-CONTRACT-001`.
- Target paths: `docs/spec/runtime/primitive-semantics.md`,
  `docs/plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md`,
  `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: implicit promotions, callable identity semantics, user-defined
  operator behavior, typeclass solver behavior, or mixed-width arithmetic.

### JN-TYPE-GRAMMAR-CLOSURE-PLAN-001

- Smallest unblocker: write an ordinary-binding generalization and defaulting
  solver contract.
- Decision needed: whether the first solver batch must include ordinary binding
  generalization, inferred class constraints, defaulting, and constrained
  signatures together.
- Recommended default: make the solver contract explicit and broad enough to
  avoid another ADT-only slice, while still splitting implementation into
  verifier-backed child rows.
- Candidate child: `JN-TYPE-SOLVER-CONTRACT-001`.
- Target paths: `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`,
  `docs/spec/semantics/bindings-and-signatures.md`,
  `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: runtime dictionary representation or abstraction method dispatch
  unless the solver contract explicitly names them as part of the first batch.

### JN-PATTERN-FUTURE-FORMS-PLAN-001

- Smallest unblocker: pick exactly one future pattern form and define its binder,
  type, and runtime contract.
- Decision needed: whether the next form is guards, or-patterns, or pattern
  synonyms.
- Recommended default: start with guards only if they do not change binder
  introduction; otherwise keep future pattern forms blocked until the solver
  contract is clearer.
- Candidate child: `JN-PATTERN-GUARD-CONTRACT-001`.
- Target paths: `docs/spec/pattern-matching-semantics.md`,
  `docs/spec/adt-pattern-semantics.md`,
  `docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`,
  `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: adding multiple pattern forms at once, generic solver behavior,
  or parser/runtime implementation before the contract lands.

### JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001

- Smallest unblocker: name one concrete CLI/runtime product delta beyond the
  closed diagnostic-only compile and `--run` baseline.
- Decision needed: whether the next product delta is examples, packaging,
  command naming, runtime output formatting, or error presentation.
- Recommended default: do not reopen runtime architecture; choose a user-visible
  CLI behavior delta with a focused `CLISpec` owner.
- Candidate child: `JN-RUNTIME-CLI-PRODUCT-DELTA-CONTRACT-001`.
- Target paths: `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`,
  `docs/jazz-language-state.md`, `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: a second backend pipeline, generated artifact output, or
  changes to compile/run semantics without a named product delta.

### JN-MODULE-REBASE-PLAN-001

- Smallest unblocker: name a future stdlib/catalog API growth contract or keep
  module/import execution closed.
- Decision needed: the exact public helper or module behavior that should grow
  next.
- Recommended default: keep module/import execution closed until a product
  feature needs new stdlib/catalog surface.
- Candidate child: `JN-STDLIB-PRELUDE-NEXT-API-CONTRACT-001`.
- Target paths: `docs/spec/stdlib-boundary.md`,
  `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`,
  `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: reworking `ModuleResolver.hs`, adding new import syntax, or
  reopening the closed module graph harness without a concrete API.

### JN-WARNING-DEPRECATED-SYNTAX-CONTRACT-001

- Smallest unblocker: either choose an accepted active syntax surface to
  deprecate or keep `W0004` reserved-only.
- Decision needed: the accepted syntax surface that should warn rather than
  error.
- Recommended default: keep `W0004` reserved-only because `trait` is permanently
  rejected and must not become compatibility syntax.
- Candidate child: `JN-WARNING-W0004-RESERVED-CLOSURE-001`.
- Target paths: `docs/spec/tooling/compiler-warning-flags.md`,
  `docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md`,
  `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: adding analyzer emission, accepting `trait`, or warning on
  syntax that the parser rejects.

### JN-WARNING-REMAINING-EMITTERS-PLAN-001

- Smallest unblocker: same as the W0004 contract above.
- Decision needed: an accepted active syntax surface that is intentionally
  deprecated.
- Recommended default: keep the emitter blocked and document reserved-only
  status.
- Candidate child: `JN-WARNING-W0004-RESERVED-CLOSURE-001`.
- Target paths: `docs/spec/tooling/compiler-warning-flags.md`,
  `docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md`,
  `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: adding an emitter without a warning surface, or treating parse
  errors as deprecation warnings.

### JN-TRAIT-CLASS-LEGACY-REBASE-001

- Smallest unblocker: close the legacy trait/class plan as reference-only after
  active `class`/`impl` child plans have superseded it.
- Decision needed: none; `trait` remains permanently rejected.
- Recommended default: do not create new implementation work from this blocker.
- Candidate child: `JN-TRAIT-CLASS-LEGACY-CLOSURE-001`.
- Target paths: `docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md`,
  `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: editing `jazz-hs/`, accepting `trait`, or adding a compatibility
  alias.

### JN-BACKEND-TARGET-LEGACY-REBASE-001

- Smallest unblocker: close the legacy backend-target plan as reference-only
  unless a new active `jazz-next` runtime product delta is selected.
- Decision needed: none while the interpreter-first product path remains the
  current baseline.
- Recommended default: keep this out of `Ready Now`; use
  `JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001` for any real product delta.
- Candidate child: `JN-BACKEND-TARGET-LEGACY-CLOSURE-001`.
- Target paths: `docs/plans/spec-clarification/2026-03-02/runtime/12-backend-target-strategy.md`,
  `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: backend implementation, codegen policy, or legacy runtime edits.

### JN-RUNTIME-INTERPRETER-LEGACY-REBASE-001

- Smallest unblocker: keep the old interpreter plan as reference-only and route
  active runtime work through the runtime product blocker.
- Decision needed: none.
- Recommended default: do not promote this blocker.
- Candidate child: `JN-RUNTIME-INTERPRETER-LEGACY-CLOSURE-001`.
- Target paths: `docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md`,
  `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: editing `jazz-hs/`, adding a second runtime path, or changing the
  active compile/run contract.

### JN-MAP-FILTER-COMPAT-PLAN-001

- Smallest unblocker: decide whether any docs-only migration work remains after
  function-first `map` and `filter` landed.
- Decision needed: compatibility policy for old examples, if any still exist.
- Recommended default: close as docs-only if searches find no active stale
  examples; do not create compiler work.
- Candidate child: `JN-MAP-FILTER-COMPAT-CLOSURE-001`.
- Target paths: `docs/plans/spec-cleanup/2026-03-02/decisions/02-map-filter-order.md`,
  `README.md`, `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: parser changes, compatibility aliases, or runtime behavior
  changes.

### JN-PARSE-ONLY-LEGACY-REBASE-001

- Smallest unblocker: write an active-path feature-resolution matrix only if a
  parse-only feature still needs implementation in `jazz-next`.
- Decision needed: which parse-only feature is still product-relevant.
- Recommended default: keep blocked until a concrete active feature is named.
- Candidate child: `JN-PARSE-ONLY-ACTIVE-MATRIX-001`.
- Target paths: `docs/feature-status.md`, `docs/jazz-language-state.md`,
  `docs/plans/spec-cleanup/2026-03-02/compiler/06-parse-only-features-resolution.md`,
  `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: editing `jazz-hs/`, reviving legacy codegen, or broad parser
  parity work.

### JN-PURITY-EFFECT-TYPING-PLAN-001

- Smallest unblocker: define the next effect-system contract beyond stub-v1
  bang-suffix enforcement.
- Decision needed: whether the next step is higher-order purity, effect types,
  cross-module purity graphs, or runtime enforcement.
- Recommended default: keep blocked until the solver and module-method contracts
  are clearer; do not add partial effect typing opportunistically.
- Candidate child: `JN-PURITY-EFFECT-CONTRACT-001`.
- Target paths: `docs/spec/semantics/purity-bang-stub-v1.md`,
  `docs/plans/spec-cleanup/2026-03-02/decisions/03-purity-bang-semantics.md`,
  `docs/execution/queue.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: runtime enforcement, inferred effects, or cross-module graph
  implementation before the contract lands.
