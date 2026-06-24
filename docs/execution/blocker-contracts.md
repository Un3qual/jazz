# Blocker Unblocker Contracts

This file turns `docs/execution/queue.md` blocked rows into promotion-ready
handoffs. Use it before opening old plan history.

When `Ready Now` is empty:

1. Read `docs/execution/queue.md`.
2. Use the ordered `Next Curation Target` candidates in that file. If that
   table is empty and the current executor status explicitly says there is no
   source-backed next curation target and no named candidate currently, stop
   after reporting that all source-backed candidates are exhausted.
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

- Smallest unblocker: none is promotion-ready after the bundled
  `Eq(Int).equals` and `Eq(Bool).equals` children. Keep the umbrella blocked
  until a separate contract narrows the next abstraction delta.
- Decision needed: choose the next abstraction contract, such as a bundled
  method-family expansion, method import/export behavior, runtime evidence, or
  dictionary/default/superclass semantics.
- Recommended default: do not promote another abstraction child from this
  umbrella without concrete syntax, target paths, runtime/type semantics, and
  focused verification.
- Candidate child: none currently.
- Target paths: not set until the next contract is accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: re-promoting completed bundled `Eq(Int).equals` or
  `Eq(Bool).equals` work, unqualified overloads, dictionary passing, runtime
  evidence values, default methods, superclasses, inferred constraints, or
  method import/export rules.

### JN-USER-DEFINED-OPERATORS-PLAN-001

- Smallest unblocker: none is promotion-ready after the Stage 2 fixed-tier
  contract and parser-only declaration child. Runtime semantics now need a
  separate executable contract before returning to `Ready Now`.
- Decision needed: define how declared operators get executable bindings,
  whether they are ordinary function values or a distinct declaration form, how
  module visibility works, and which type/runtime checks close the first
  execution slice.
- Recommended default: keep Stage 2 parser metadata complete and block runtime
  operator semantics until a narrow contract excludes custom precedence,
  custom associativity, new built-ins, and runtime overload dispatch.
- Candidate child: none currently.
- Target paths: not set until the runtime/operator-semantics contract is
  accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: re-promoting the completed fixed-tier contract or parser child,
  custom precedence declarations, new builtin operators, runtime overload
  dispatch, or parser syntax already covered by
  `JN-OPERATORS-STAGE2-FIXED-TIER-PARSER-001`.

### JN-PRIMITIVE-SURFACE-EXPANSION-PLAN-001

- Smallest unblocker: none is promotion-ready after the literal-suffix contract
  was accepted and `JN-PRIMITIVE-FRACTIONAL-LITERAL-SUFFIXES-001` was promoted.
- Decision needed: a separate contract for implicit integer-to-float promotion,
  implicit mixed-width arithmetic/comparison behavior, or solver/defaulting
  behavior before any post-suffix primitive child can return to `Ready Now`.
- Recommended default: do not promote another primitive child from this umbrella
  until implicit promotion, mixed-width behavior, or broader solver semantics
  are accepted with concrete target paths and focused verification.
- Candidate child: none currently.
- Target paths: not set until the next post-suffix primitive contract is
  accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: re-promoting `JN-PRIMITIVE-LITERAL-SUFFIX-CONTRACT-001`,
  changing the queued suffix child, callable identity semantics, user-defined
  operator behavior, implicit promotions, typeclass solver behavior, or
  mixed-width arithmetic before those contracts are accepted.

### JN-TYPE-GRAMMAR-CLOSURE-PLAN-001

- Smallest unblocker: promote the next verifier-backed implementation child
  from the accepted `JN-TYPE-SOLVER-CONTRACT-001` slices.
- Decision needed: choose one remaining solver slice narrow enough for a child
  row. The ordinary binding schemes/per-use instantiation child has landed.
- Recommended default: keep any next executable child limited to one remaining
  solver slice. Do not batch inferred class constraints, broad defaulting,
  solver-backed constrained signatures, runtime dictionaries, explicit type
  application, or primitive mixed-width behavior together.
- Candidate child: none currently; the next curation pass must add a concrete
  row before implementation resumes.
- Target paths: update once a new child is promoted in
  `docs/execution/queue.md#ready-now`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: re-promoting `JN-TYPE-SOLVER-CONTRACT-001`, runtime dictionary
  representation, abstraction method dispatch, explicit type application,
  higher-rank polymorphism, module/import behavior, primitive mixed-width or
  implicit promotion, or any `jazz-hs`/`jazz2` work.

### JN-PATTERN-FUTURE-FORMS-PLAN-001

- Smallest unblocker: none is promotion-ready after the guard contract and
  implementation child landed. A future child must pick exactly one remaining
  pattern form and define its binder, type, and runtime contract.
- Decision needed: whether the next form is or-patterns or pattern synonyms,
  and how its binder compatibility rules work.
- Recommended default: keep future pattern forms blocked until one remaining
  form has concrete syntax, binder compatibility, target paths, and focused
  verification.
- Candidate child: none currently.
- Target paths: not set until the next future-pattern contract is accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: re-promoting guards, adding multiple pattern forms at once,
  generic solver behavior, or parser/runtime implementation before a contract
  lands.

### JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001

- Smallest unblocker: none is promotion-ready after the CLI help output child
  landed.
- Decision needed: choose a later runtime product delta separately if product
  work continues.
- Recommended default: keep the compile/run/help baseline closed. Do not reopen
  runtime architecture, compile output, run output, packaging, generated
  artifacts, or backend generation.
- Candidate child: none currently.
- Target paths: not set until the next product delta is accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: a second backend pipeline, generated artifact output, a bare
  `help` subcommand, or changes to compile/run/help semantics without a new
  contract.

### JN-MODULE-REBASE-PLAN-001

- Smallest unblocker: none currently after the stdlib/prelude next API
  candidate validation; keep module/import execution closed until a concrete
  future stdlib/catalog API or module behavior is named.
- Decision needed: the exact public stdlib/catalog API or module behavior that
  should grow next, plus its runtime/API contract.
- Recommended default: keep module/import execution closed until a product
  feature needs new stdlib/catalog surface.
- Candidate child: none currently.
- Target paths: not set until a concrete API/runtime contract exists.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: reworking `ModuleResolver.hs`, adding new import syntax,
  reopening the closed module graph harness, adding new prelude/catalog API
  without a named contract, adding direct public builtin fallback in no-prelude
  mode, or adding package/module-root semantics.

### JN-WARNING-DEPRECATED-SYNTAX-CONTRACT-001

- Smallest unblocker: none for the current active language surface; the W0004
  reserved-only closure landed as `JN-WARNING-W0004-RESERVED-CLOSURE-001`.
- Decision needed: none until a future accepted active syntax surface is
  intentionally deprecated.
- Recommended default: keep `W0004` reserved-only because `trait` is
  permanently rejected and must not become compatibility syntax.
- Candidate child: none currently.
- Target paths: not set until a future accepted-surface contract exists.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: adding analyzer emission, accepting `trait`, or warning on
  syntax that the parser rejects.

### JN-WARNING-REMAINING-EMITTERS-PLAN-001

- Smallest unblocker: none for the current active language surface; future
  W0004 emitter work needs a new accepted-surface contract.
- Decision needed: choose an accepted active syntax surface that is
  intentionally deprecated.
- Recommended default: keep the emitter unpromoted until the syntax surface,
  warning payload, target paths, and focused verification exist.
- Candidate child: none currently.
- Target paths: not set until a future accepted-surface contract exists.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: adding an emitter without a warning surface, or treating parse
  errors as deprecation warnings.

### JN-TRAIT-CLASS-LEGACY-REBASE-001

- Smallest unblocker: none; the legacy trait/class cleanup plan is closed as
  reference-only by `JN-TRAIT-CLASS-LEGACY-CLOSURE-001`.
- Decision needed: none; active `jazz-next` permanently rejects
  declaration-shaped `trait` syntax and uses canonical `class`/`impl`.
- Recommended default: do not create new implementation work from this blocker.
- Candidate child: none currently.
- Target paths: not set; future abstraction work should use the active
  abstraction semantics blockers and `jazz-next` target paths.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: editing `jazz-hs/`, accepting `trait`, or adding a compatibility
  alias.

### JN-BACKEND-TARGET-LEGACY-REBASE-001

- Smallest unblocker: none; the legacy backend-target plan is closed as
  reference-only by `JN-BACKEND-TARGET-LEGACY-CLOSURE-001`.
- Decision needed: none while the interpreter-first product path remains the
  current baseline.
- Recommended default: keep this out of `Ready Now`; use
  `JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001` for any real product delta.
- Candidate child: none currently.
- Target paths: not set; future runtime product work should use active
  `jazz-next` runtime product contracts and target paths.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: backend implementation, codegen policy, or legacy runtime edits.

### JN-RUNTIME-INTERPRETER-LEGACY-REBASE-001

- Smallest unblocker: none; the old interpreter plan is closed as
  reference-only by `JN-RUNTIME-INTERPRETER-LEGACY-CLOSURE-001`.
- Decision needed: none.
- Recommended default: do not promote this blocker.
- Candidate child: none currently.
- Target paths: not set; future runtime product work should use active
  `jazz-next` runtime product contracts and target paths.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: editing `jazz-hs/`, adding a second runtime path, or changing the
  active compile/run contract.

### JN-MAP-FILTER-COMPAT-PLAN-001

- Smallest unblocker: none; active examples/specs/tests no longer require
  collection-first compatibility, and the legacy cleanup item was closed by
  `JN-MAP-FILTER-COMPAT-CLOSURE-001`.
- Decision needed: none.
- Recommended default: do not promote this blocker.
- Candidate child: none currently.
- Target paths: not set; future collection primitive work should use active
  `jazz-next` primitive, stdlib-boundary, or runtime-product contracts.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: parser changes, compatibility aliases, or runtime behavior
  changes.

### JN-PARSE-ONLY-LEGACY-REBASE-001

- Smallest unblocker: none; the active matrix check found no standalone
  implementation-ready `jazz-next` parse-only feature, and the legacy cleanup
  item was closed by `JN-PARSE-ONLY-ACTIVE-MATRIX-001`.
- Decision needed: none.
- Recommended default: do not promote this blocker.
- Candidate child: none currently.
- Target paths: not set; future parser-only or parser-mostly surfaces should
  use their owning active blockers and contracts.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: editing `jazz-hs/`, reviving legacy codegen, or broad parser
  parity work.

### JN-PURITY-EFFECT-TYPING-PLAN-001

- Smallest unblocker: none currently; the active evidence refresh in
  `JN-PURITY-EFFECT-CONTRACT-001` kept broader effect typing blocked pending
  remaining solver and module-method/export/runtime-evidence contracts.
- Decision needed: none until those prerequisite contracts are clearer.
- Recommended default: do not promote partial effect typing opportunistically.
- Candidate child: none currently.
- Target paths: not set until a future effect-system contract is accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: runtime enforcement, inferred effects, effect types,
  cross-module purity graphs, or effect typing in signatures before a future
  contract lands.
