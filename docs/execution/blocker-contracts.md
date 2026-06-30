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

- Smallest unblocker: promote the accepted bundled-prelude `Eq(Float64).equals`
  child under the explicit `Float`/`Float64` alias-overlap policy.
- Decision needed: accepted on `2026-06-30`: default bundled `Eq(Float)` and
  `Eq(Float64)` method facts may coexist only because `Float` is the public
  alias for `Float64`; non-alias duplicate visible impl facts continue to
  reject.
- Recommended default: promote exactly the `Eq(Float64).equals` child next,
  then keep the abstraction umbrella blocked for dictionaries, runtime
  evidence values, default methods, superclasses, inferred constraints, new
  bundled method families, and method import/export rules.
- Candidate child: `JN-ABSTRACTION-BUNDLED-PRELUDE-EQ-FLOAT64-METHOD-001`.
- Target paths: `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`;
  `jazz-next/stdlib/Prelude.jz`;
  `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`;
  `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`;
  `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`;
  `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`;
  `docs/spec/abstractions/capability-model.md`;
  `docs/plans/2026-06-30-jazz-next-bundled-prelude-eq-float64-method.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`; `git diff --check`; plus the focused
  `PreludeLoadingSpec.hs`, `BindingSignatureCoherenceSpec.hs`,
  `RuntimeSemanticsSpec.hs`, and `BuiltinCatalogSpec.hs` commands named by the
  child plan.
- Not in scope: re-promoting completed bundled `Eq(Int).equals`,
  `Eq(Bool).equals`, `Eq(Float).equals`, `Eq(Float16).equals`, or
  `Eq(Float32).equals` work; unqualified overloads, dictionary passing,
  runtime evidence values, default methods, superclasses, inferred
  constraints, non-alias overlap/orphan behavior, or method import/export
  rules.

### JN-USER-DEFINED-OPERATORS-PLAN-001

- Smallest unblocker: promote the first accepted post-binding operator child:
  operator-specific adjacent signatures for same-source `(op) = <expr>.`
  bindings.
- Decision needed: accepted on `2026-06-30`: plan operator-specific type
  signatures, custom precedence, and custom associativity as separate child
  rows. Execute signatures first, then custom precedence, then custom
  associativity.
- Recommended default: keep Stage 2 fixed-tier parsing and same-source
  `(op) = <expr>.` execution complete. Promote only the signature child first;
  do not batch custom precedence or associativity into that child.
- Candidate child: `JN-OPERATORS-SPECIFIC-TYPE-SIGNATURES-001`.
- Target paths: `docs/spec/syntax/operators.md`;
  `jazz-next/src/JazzNext/Compiler/Parser.hs`;
  `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`;
  `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`;
  `jazz-next/src/JazzNext/Compiler/TypeInference.hs`;
  `jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs`;
  `jazz-next/test/JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs`;
  `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`;
  `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`;
  `docs/plans/2026-06-30-jazz-next-operator-signatures-precedence-associativity.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`; plus the focused `OperatorFixitySpec.hs`,
  `OperatorInvalidSyntaxSpec.hs`, `PrimitiveSemanticsSpec.hs`, and
  `RuntimeSemanticsSpec.hs` commands named by the child plan.
- Not in scope: re-promoting the completed fixed-tier contract or parser child,
  re-promoting `JN-OPERATORS-DECLARED-FUNCTION-BINDINGS-001`, custom
  precedence declarations in the signature child, custom associativity in the
  signature or precedence child, new builtin operators, runtime overload
  dispatch, cross-module operator APIs, or parser syntax already covered by
  `JN-OPERATORS-STAGE2-FIXED-TIER-PARSER-001`.

### JN-PRIMITIVE-SURFACE-EXPANSION-PLAN-001

- Smallest unblocker: promote the accepted direct typed integer-to-`Float64`
  promotion child.
- Decision needed: accepted on `2026-06-30`: add direct binary typed
  integer-to-`Float`/`Float64` promotion for arithmetic, comparison, and
  equality only. Keep `Float16`/`Float32`, mixed concrete float widths,
  operator values, sections, user-defined operators, and broader
  solver/defaulting behavior out of scope.
- Recommended default: promote exactly the direct typed integer-to-`Float64`
  child, then keep mixed-width float behavior and broader solver/defaulting
  behavior blocked behind their own contracts.
- Candidate child: `JN-PRIMITIVE-TYPED-INT-TO-FLOAT64-PROMOTION-001`.
- Target paths: `docs/spec/runtime/primitive-semantics.md`;
  `jazz-next/src/JazzNext/Compiler/TypeInference.hs`;
  `jazz-next/src/JazzNext/Compiler/Runtime.hs`;
  `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`;
  `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`;
  `docs/plans/2026-06-30-jazz-next-typed-int-to-float64-promotion.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`; plus the focused `PrimitiveSemanticsSpec.hs`
  and `RuntimeSemanticsSpec.hs` commands named by the child plan.
- Not in scope: re-promoting `JN-PRIMITIVE-LITERAL-SUFFIX-CONTRACT-001`,
  changing the completed suffix or Float64 integer-literal children, callable
  identity semantics, user-defined operator behavior, typeclass solver
  behavior, implicit promotion into `Float16` or `Float32`, mixed-width
  arithmetic, or operator value/section promotion.

### JN-TYPE-GRAMMAR-CLOSURE-PLAN-001

- Smallest unblocker: use the accepted remaining-solver slice plan and promote
  the first implementation child, inferred class constraints, when a type
  solver row is selected.
- Decision needed: accepted on `2026-06-30`: write the remaining solver plan for
  inferred class constraints, final defaulting/ambiguity, explicit type
  application, and runtime evidence/dictionaries, but keep them as separate
  verifier-backed child rows.
- Recommended default: promote inferred class constraints first. Do not batch
  inferred class constraints, broad defaulting, runtime evidence/dictionaries,
  explicit type application, primitive mixed-width behavior, or typed
  integer-to-float promotion together.
- Candidate child: `JN-TYPE-SOLVER-INFERRED-CLASS-CONSTRAINTS-001`.
- Target paths: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`;
  `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`;
  `docs/spec/semantics/bindings-and-signatures.md`;
  `docs/plans/2026-06-30-jazz-next-type-solver-remaining-slices.md`.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`; plus the focused
  `BindingSignatureCoherenceSpec.hs` command named by the remaining-slices
  plan.
- Not in scope: re-promoting `JN-TYPE-SOLVER-CONTRACT-001`, re-promoting
  `JN-TYPE-SOLVER-ORDINARY-BINDING-SCHEMES-001` or
  `JN-TYPE-SOLVER-CONSTRAINED-SIGNATURE-SCHEMES-001`, runtime dictionary
  representation, abstraction method dispatch, explicit type application,
  higher-rank polymorphism, module/import behavior, primitive mixed-width or
  implicit promotion, or any `jazz-hs`/`jazz2` work.

### JN-PATTERN-FUTURE-FORMS-PLAN-001

- Smallest unblocker: none is promotion-ready after guard-only case-arm
  semantics, top-level case-arm or-patterns, and lambda-parameter or-patterns
  landed.
- Decision needed: define a separate pattern-synonym contract if future pattern
  forms should continue.
- Recommended default: keep pattern synonyms blocked until they have a concrete
  binder/type/runtime contract, syntax, target paths, and focused verification.
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
  remaining defaulting, module-method/export, and runtime-evidence contracts.
- Decision needed: none until those prerequisite contracts are clearer.
- Recommended default: do not promote partial effect typing opportunistically.
- Candidate child: none currently.
- Target paths: not set until a future effect-system contract is accepted.
- Verification: `bash scripts/check-execution-queue.sh`;
  `bash scripts/check-docs.sh`.
- Not in scope: runtime enforcement, inferred effects, effect types,
  cross-module purity graphs, or effect typing in signatures before a future
  contract lands.
