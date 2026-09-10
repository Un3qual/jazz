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

- Smallest unblocker: none currently. The cross-cutting typed module export
  inventory landed under module ownership as
  `JN-MODULE-TYPED-EXPORT-INVENTORY-001`; there is no separate abstraction
  child to promote.
- Decision needed: none for that child. RFC 0017 additionally implements
  alias-qualified classes in Haskell. Preserve qualified and unqualified class
  capability imports, class-attached impl payloads,
  and non-transitive module boundaries.
- Recommended default: keep the landed typed inventory behavior. Keep
  user-visible dictionaries, dictionary optimization, default methods,
  superclasses, new bundled method families,
  re-exports, and new impl policy blocked behind separate contracts.
- Candidate child: none currently.
- Target paths: not set; no independent abstraction paths are open after the
  typed inventory child landed.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: re-promoting completed bundled `Eq(Int).equals`,
  `Eq(Bool).equals`, `Eq(Float).equals`, `Eq(Float16).equals`,
  `Eq(Float32).equals`, or `Eq(Float64).equals` work; unqualified overloads,
  dictionary passing/optimization, default methods, superclasses, non-alias
  overlap/orphan behavior, re-exports, or any method visibility expansion
  beyond the landed typed inventory and RFC 0017 contracts. Hosted parity
  for RFC 0017 remains deferred by the maintainer.

### JN-USER-DEFINED-OPERATORS-PLAN-001

- Smallest unblocker: none currently promotion-ready after custom
  associativity landed.
- Decision needed: accepted on `2026-06-30`: plan operator-specific type
  signatures, custom precedence, and custom associativity as separate child
  rows. Operator signatures, custom precedence, and custom associativity have
  landed; no later operator child has an accepted executable contract.
- Recommended default: keep Stage 2 fixed-tier parsing, same-source
  `(op) = <expr>.` execution, adjacent operator signatures, and custom numeric
  precedence, and explicit custom associativity complete. Do not promote
  runtime overload dispatch, cross-module APIs, or new built-ins without a new
  contract.
- Candidate child: none currently.
- Target paths: not set until the next operator contract is accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: re-promoting the completed fixed-tier contract or parser child,
  re-promoting `JN-OPERATORS-DECLARED-FUNCTION-BINDINGS-001`, re-promoting
  `JN-OPERATORS-SPECIFIC-TYPE-SIGNATURES-001`, re-promoting
  `JN-OPERATORS-CUSTOM-PRECEDENCE-001`, re-promoting
  `JN-OPERATORS-CUSTOM-ASSOCIATIVITY-001`, new precedence ranges, new builtin
  operators, runtime overload dispatch, cross-module operator APIs, or parser
  syntax already covered by completed operator children.

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

- Smallest unblocker: none currently after runtime evidence/dictionaries
  landed.
- Decision needed: accepted on `2026-06-30`: write the remaining solver plan for
  inferred class constraints, final defaulting/ambiguity, explicit type
  application, and runtime evidence/dictionaries as separate verifier-backed
  child rows. All accepted children from that plan are now landed.
- Recommended default: keep type grammar closure blocked until a new concrete
  contract exists; do not promote default methods, superclasses, module method
  import/export behavior, orphan/overlap changes, dictionary optimization,
  primitive mixed-width behavior, typed integer-to-float promotion, or another
  broad type-system batch opportunistically.
- Candidate child: none currently.
- Target paths: not set until the next type-system contract is accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: re-promoting `JN-TYPE-SOLVER-CONTRACT-001`, re-promoting
  `JN-TYPE-SOLVER-ORDINARY-BINDING-SCHEMES-001` or
  `JN-TYPE-SOLVER-CONSTRAINED-SIGNATURE-SCHEMES-001`, re-promoting
  `JN-TYPE-SOLVER-INFERRED-CLASS-CONSTRAINTS-001`, re-promoting
  `JN-TYPE-SOLVER-FINAL-DEFAULTING-AMBIGUITY-001`, re-promoting
  `JN-TYPE-SOLVER-EXPLICIT-TYPE-APPLICATION-001`, re-promoting
  `JN-TYPE-SOLVER-RUNTIME-EVIDENCE-DICTIONARIES-001`, default methods,
  superclasses, abstraction method import/export behavior, higher-rank
  polymorphism, primitive mixed-width or implicit promotion, or revival of
  removed legacy implementations.

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

- Smallest unblocker: none currently. The namespace-aware module export child
  landed as `JN-MODULE-NAMESPACE-AWARE-EXPORT-001`, followed by Haskell
  alias-qualified classes under `JN-MODULE-ALIAS-QUALIFIED-CLASSES-001` (RFC 0017).
- Decision needed: none. Optional namespace prefixes, bare-selector
  compatibility, omitted-list export-all, `()` export-nothing, local/public
  inventory separation, alias-qualified methods/constraints/impl heads, and
  no re-exports are implemented. Hosted qualification parity is explicitly
  deferred, including existing parser comparisons affected by the new grammar.
- Recommended default: preserve the completed namespace-aware export contract
  and `E4007`-`E4015` diagnostics until a separate source-backed module behavior
  contract is accepted.
- Candidate child: none currently.
- Target paths: not set until a separate module behavior contract is accepted.
- Verification: focused `ModuleImportParserSpec.hs`, `ParserFoundationSpec.hs`,
  `OperatorFixitySpec.hs`, `ModuleExportsSpec.hs`,
  `ModuleResolutionSpec.hs`, `ModulePipelineContractSpec.hs`, and `LoaderSpec.hs`;
  `cabal build all`;
  `cabal test all --test-show-details=failures`;
  `bash scripts/check-execution-queue.sh`; `bash scripts/check-docs.sh`;
  `git diff --check`.
- Landed evidence: `src/Jazz/Compiler/ModuleExports.hs` owns the
  typed inventory and structured selectors; module headers accept exact
  `value`, `constructor`, `type`, and `class` prefixes plus bare compatibility;
  `src/Jazz/Compiler/ModuleResolver.hs` separates local and public
  inventories; compiler imports and runtime publication consume the public
  inventory; focused and full verification passed on `2026-07-10`.
- Not in scope: re-exports, wildcard or constructor-group shorthand, body-level
  export declarations, visibility modifiers, cross-module operators,
  separate impl imports, orphan/overlap policy,
  default methods, superclasses, effects, new prelude/catalog API, public
  builtin fallback in no-prelude mode, or package/module-root semantics.

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
