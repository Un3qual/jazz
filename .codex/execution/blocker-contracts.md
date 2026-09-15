# Blocker Unblocker Contracts

This file turns `.codex/execution/queue.md` blocked rows into promotion-ready
handoffs. Use it before opening old plan history.

Use `docs/project/status.md` for shipped status. Public behavior belongs in
`docs/language/` and `docs/reference/`; accepted architectural boundaries live
in `rfcs/accepted/`.

When `Ready Now` is empty:

1. Read `.codex/execution/queue.md`.
2. Use the ordered `Next Curation Target` candidates in that file. If that
   table is empty and the current executor status explicitly says there is no
   source-backed next curation target and no named candidate currently, stop
   after reporting that all source-backed candidates are exhausted.
3. Open only the matching section below and its named active plan, public
   owner, or accepted RFC.
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

### JN-BOOTSTRAP-INTERPRETER-PROFILE-PLAN-001

- Retained implementation: the working Haskell compiler and analyzed-core
  interpreter, shared semantic types, nominal identities, runtime plans,
  standard library, runtime host boundary, and stack-safe evaluation machine.
  Jazz-authored lexing, parsing, and canonical-core lowering retain structural
  differential coverage against stage 0.
- Accepted frontend decision: [RFC 0004](../../rfcs/accepted/0004-hosted-canonical-compiler.md).
  Its backend requirements are superseded by
  [RFC 0016](../../rfcs/accepted/0016-optional-backend-removal.md), which removes
  the optional Typed Core/Lowered IR backend and its mirrors. Historical backend
  child plans do not authorize restoring or extending it.
- Verified removal: [approved removal plan](../plans/2026-09-07-remove-optional-backend.md).
  The completed [architecture plan](../plans/2026-08-31-jazz-compiler-architecture-simplification.md)
  remains implementation history; its backend and interpreter-cutover proposals
  are superseded. The analyzed-core interpreter remains the execution path.
- Smallest unblocker: choose a concrete hosted-compiler or native-execution goal
  following the completed removal.
- Decision needed: the executable goal, integration boundary, and acceptance
  evidence for that goal. Native work requires a fresh accepted design.
- Recommended default: keep the tested compiler and hosted frontend; do not
  restore speculative representations or schedule a second interpreter.
- Candidate child: none. The approved removal is complete at `6a646a18`,
  with all 60 retained suites and the isolated Nix gate passing.
- Target paths: determined by a new concrete goal, not the retired backend.
- Verification: a new child must name focused behavioral and integration checks.
- Not in scope: automatic interpreter cutover, restoring IR schema mirroring,
  native target selection, or language changes without a separate decision.

### JN-ABSTRACTION-SEMANTICS-PLAN-001

- Completed accepted contract: [RFC 0019](../../rfcs/accepted/0019-generic-capabilities-and-library-names.md), with compiler and library implementation through `99a15625`.
- Retained baseline: one-parameter generic classes, inferred constructor kinds,
  ordinary methods, defaults, superclasses, transitive instance transport,
  bundled collection capabilities, all 183 public renames, Text mapping, and
  explicit-import Reduce. All required verification gates passed.
- Smallest unblocker: none; no source-backed next curation target remains.
- Decision needed: a separate accepted contract for further capability work.
- Ready child: none.
- Candidate child: none.
- Implementation receipt: [consolidated capabilities and operators plan](../plans/2026-09-13-generic-capabilities-and-library-names.md).
- Deferred: functional dependencies, associated types, multi-parameter classes,
  automatic destination selection, Empty, parenthesized application heads,
  new class selectors, and broader hosted compiler work.

### JN-USER-DEFINED-OPERATORS-PLAN-001

- Current behavior: source-local declarations, signatures, precedence,
  associativity, and RFC 0020 ordinary function dispatch are implemented.
- Smallest unblocker: review the detailed combined
  [RFC 0021](../../rfcs/proposed/0021-module-reexports-and-operator-transport.md).
- Decision needed: explicit operator export syntax, qualified notation,
  defining-module fixity, and the discovery-before-body-parse boundary.
- Recommended default: execute the single combined module API plan after RFC
  acceptance; reuse existing compiler records, names, tables, and runtime cells.
- Candidate child: `JN-MODULE-API-COMPOSITION-001`, shared with
  [the module umbrella](#jn-module-rebase-plan-001). Do not create a second row.
- Target paths and verification: the shared
  [implementation plan](../plans/2026-09-15-module-reexports-and-operator-transport.md)
  owns the exact file list and focused/full commands.
- Not in scope: new operator spellings, new precedence ranges, a separate
  operator runtime, or restoring the removed backend.

### JN-PRIMITIVE-SURFACE-EXPANSION-PLAN-001

- Smallest unblocker: none currently promotion-ready after the direct typed
  integer-to-`Float`/`Float64` promotion child landed.
- Decision needed: choose a later primitive delta separately. Direct binary
  typed integral promotion is now implemented only for arithmetic, comparison,
  and equality with a `Float`/`Float64` peer.
- Recommended default: keep mixed-width float behavior, `Float16`/`Float32`
  promotion, operator-value or section promotion, user-defined operator
  behavior, and broader solver/defaulting behavior blocked behind their own
  contracts.
- Candidate child: none currently.
- Target paths: not set until the next primitive contract is accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: re-promoting `JN-PRIMITIVE-LITERAL-SUFFIX-CONTRACT-001`,
  changing the completed suffix, Float64 integer-literal, or typed
  integer-to-Float64 children, callable identity semantics, user-defined
  operator behavior, typeclass solver behavior, implicit promotion into
  `Float16` or `Float32`, mixed-width arithmetic, or operator value/section
  promotion.

### JN-TYPE-GRAMMAR-CLOSURE-PLAN-001

- Retained baseline: ordinary schemes, inferred constraints, numeric defaulting,
  explicit type application, analyzed runtime evidence, constructor kinds,
  canonical applications, generic heads, prerequisite solving, rigid method
  checking, superclass evidence, and rejection of overlapping visible heads.
  RFC 0019 and its library integration are complete.
- Smallest unblocker: none; no independently accepted solver child remains.
- Decision needed: a separate accepted contract for further solver work.
- Candidate child: none.
- Deferred: functional dependencies, associated types, higher-rank types,
  numeric promotion changes, accepting overlapping instances, and speculative rewrites.

### JN-PATTERN-FUTURE-FORMS-PLAN-001

- Smallest unblocker: none is promotion-ready after guard-only case-arm
  semantics, top-level case-arm or-patterns, and lambda-parameter or-patterns
  landed.
- Decision needed: deferred by maintainer on `2026-06-30`; keep pattern
  synonyms blocked for now. Define a separate pattern-synonym contract only if
  future pattern forms are explicitly reopened.
- Recommended default: keep pattern synonyms blocked with no candidate child
  until a concrete binder/type/runtime contract, syntax, target paths, and
  focused verification are accepted.
- Candidate child: none currently.
- Target paths: not set until the next pattern-synonym or future-form contract
  is accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: re-promoting guards, top-level case-arm or-patterns, or
  lambda-parameter or-patterns, adding multiple pattern forms at once, pattern
  synonyms without a concrete contract, nested/grouped or-patterns, lambda
  guards, generic solver behavior, or any legacy compiler work.

### JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001

- Smallest unblocker: none is promotion-ready after the CLI help output child
  and explicit `-` stdin source selector child landed.
- Decision needed: choose a later runtime product delta separately if product
  work continues.
- Recommended default: keep the compile/run/help/stdin-selector baseline
  closed. Do not reopen runtime architecture, compile output, run output, stdin
  source selection, packaging, generated artifacts, or backend generation.
- Candidate child: none currently.
- Target paths: not set until the next product delta is accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: a second backend pipeline, generated artifact output, a bare
  `help` subcommand, or changes to compile/run/help/stdin semantics without a
  new contract.

### JN-MODULE-REBASE-PLAN-001

- Current behavior: namespace-aware explicit exports, grouped constructor
  selectors, private/public separation, alias-qualified classes, and transitive
  implementations are implemented. Imports cannot yet be re-exported.
- Smallest unblocker: accept the detailed
  [RFC 0021](../../rfcs/proposed/0021-module-reexports-and-operator-transport.md),
  then promote its single combined implementation candidate.
- Decision needed: the batch direction is approved. Review exact selector and
  operator qualification syntax, default operator privacy, identity/conflict
  rules, and the explicit diagnostic-order change before implementation.
- Recommended default: extend the existing module records with original public
  names and exported fixity. Reuse the existing reference map, selector types,
  operator payloads, dependency DFS, typed interfaces, and runtime cells.
- Candidate child: `JN-MODULE-API-COMPOSITION-001`.
- Plan: [module re-exports and operator transport](../plans/2026-09-15-module-reexports-and-operator-transport.md).
- Target paths: the complete `target_paths` list in the
  [implementation plan](../plans/2026-09-15-module-reexports-and-operator-transport.md),
  mirrored in the curation row. It covers the existing Haskell and hosted
  compiler owners, behavioral tests, examples, public docs, and dispatcher
  closeout for Tasks 1-5.
- Verification: `bash scripts/check-execution-queue.sh`;
  `python3 scripts/check-rfcs.py .`; `bash scripts/check-docs.sh` for design
  publication. Implementation requires the focused module/operator and hosted
  suites, full-scale parser checks, Haskell quality, and the full serialized
  main gate; exact commands are recorded in the plan and curation row.
- Not in scope: whole-module wildcard re-exports, renamed exports, new operator
  characters, cyclic modules, package resolution, effect-system changes, or
  unrelated compiler representations. Existing constructor-group selectors are
  supported by the proposed re-export contract.

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
- Decision needed: none; active `Jazz` permanently rejects
  declaration-shaped `trait` syntax and uses canonical `class`/`impl`.
- Recommended default: do not create new implementation work from this blocker.
- Candidate child: none currently.
- Target paths: not set; future abstraction work should use the active
  abstraction semantics blockers and root target paths.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: reviving removed legacy code, accepting `trait`, or adding a compatibility
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
  `Jazz` runtime product contracts and target paths.
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
  `Jazz` runtime product contracts and target paths.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: reviving removed legacy code, adding a second runtime path, or changing the
  active compile/run contract.

### JN-MAP-FILTER-COMPAT-PLAN-001

- Smallest unblocker: none; active examples/specs/tests no longer require
  collection-first compatibility, and the legacy cleanup item was closed by
  `JN-MAP-FILTER-COMPAT-CLOSURE-001`.
- Decision needed: none.
- Recommended default: do not promote this blocker.
- Candidate child: none currently.
- Target paths: not set; future collection primitive work should use active
  `Jazz` primitive, stdlib-boundary, or runtime-product contracts.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: parser changes, compatibility aliases, or runtime behavior
  changes.

### JN-PARSE-ONLY-LEGACY-REBASE-001

- Smallest unblocker: none; the active matrix check found no standalone
  implementation-ready `Jazz` parse-only feature, and the legacy cleanup
  item was closed by `JN-PARSE-ONLY-ACTIVE-MATRIX-001`.
- Decision needed: none.
- Recommended default: do not promote this blocker.
- Candidate child: none currently.
- Target paths: not set; future parser-only or parser-mostly surfaces should
  use their owning active blockers and contracts.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: reviving removed legacy code or codegen, or broad parser
  parity work.

### JN-PURITY-EFFECT-TYPING-PLAN-001

- Smallest unblocker: none currently; broader effect typing remains blocked
  after final defaulting and compiler-owned runtime evidence landed.
- Decision needed: none until module-method/export behavior and a concrete
  effect-system contract are clearer.
- Recommended default: do not promote partial effect typing opportunistically.
- Candidate child: none currently.
- Target paths: not set until a future effect-system contract is accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: runtime enforcement, inferred effects, effect types,
  cross-module purity graphs, or effect typing in signatures before a future
  contract lands.
