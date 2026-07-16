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

### JN-BOOTSTRAP-INTERPRETER-PROFILE-PLAN-001

- Completed children: `JN-BOOTSTRAP-MAYBE-RESULT-LIBRARIES-001` added ordinary
  `Maybe` and `Result` modules, and `JN-BOOTSTRAP-TEXT-TRAVERSAL-001` added the
  explicit-import scalar traversal API with private backend-neutral adapters.
  `JN-BOOTSTRAP-HOST-TEXT-IO-001` added explicit-import `IO`/`IOError`, seven
  private host bridges, the typed monadic runtime-host seam, strict UTF-8
  production operations, arguments, and exit without coupling the public
  contract to Haskell or LLVM. `JN-BOOTSTRAP-STACK-SAFE-EVALUATION-001` added
  the shared explicit evaluator machine and proved pure, host-backed, and
  imported closure depth floors without introducing bytecode or LLVM coupling.
  `JN-BOOTSTRAP-CANONICAL-COMPARISON-001` preserved structured stage-0 lexical
  failures, added the ordinary Jazz-owned canonical ADTs and test-only Haskell
  adapter, and established the explicit accepted/rejected parser corpus.
  `JN-BOOTSTRAP-JAZZ-LEXER-001` added permanent generic list construction and
  Unicode scalar/text services, ordinary `List`/`Char`/`Text` modules, and the
  two-space-indented Jazz lexer; it matches all 333 canonical cases
  deterministically and completes 20,000-character and 10,000-token traversal
  floors without host stack growth. `JN-BOOTSTRAP-JAZZ-PARSER-DESIGN-001`
  accepted the fail-fast surface-AST, structured-failure, façade, parser-kernel,
  and ordered grammar-slice contract on `2026-07-16`.
- Accepted decision: the parser contract is
  `2026-07-12-jazz-next-bootstrap-jazz-parser-design.md`. It compares the
  complete ordinary surface AST before lowering, preserves structured failures
  before the unified `Diagnostic`, uses canonical lexer tokens as the primary
  input, and starts with a generic compiler-local parser kernel. Existing Jazz
  collection/text APIs are sufficient.
- Smallest unblocker: implement
  `JN-BOOTSTRAP-JAZZ-PARSER-FOUNDATION-001`, the contract-and-kernel child now
  promoted in `Ready Now`.
- Decision needed: none for the promoted child. The accepted design and
  matching plan fix its schema, ownership, failure, consumption, determinism,
  progress, and large-input boundaries.
- Recommended default: preserve structured stage-0 parser failures behind the
  existing diagnostic API, define the complete ordinary Jazz surface schema
  plus total test-only Haskell adapter, and implement the generic parser kernel
  without substantive Jazz grammar.
- Candidate child: `JN-BOOTSTRAP-JAZZ-PARSER-FOUNDATION-001`, the sole promoted
  implementation child. Expression grammar and all later slices remain
  unpromoted.
- Target paths: `docs/execution/blocker-contracts.md`,
  `docs/execution/queue.md`,
  `docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md`,
  `docs/superpowers/specs/2026-07-12-jazz-next-bootstrap-jazz-parser-design.md`,
  `docs/superpowers/plans/2026-07-16-jazz-next-bootstrap-parser-foundation.md`,
  the active Haskell parser owners, `jazz-next/jazz/compiler/ParserTypes.jz`,
  `jazz-next/jazz/compiler/ParserCore.jz`, bootstrap comparison/kernel tests,
  and `jazz-next/jazz-next.cabal`.
- Verification: the focused canonical parser/kernel/lexer/repository suites;
  development-warning build; full Cabal suite; queue/docs validators; and
  `git diff --check`, as listed exactly in the child plan.
- Not in scope: Jazz grammar; a public stdlib parser API; parser-specific host
  intrinsics or Haskell callbacks; recovery or partial ASTs; canonical core;
  bytecode or a VM; lowered IR; LLVM emission; object generation; linking; or
  native-runtime implementation.

### JN-ABSTRACTION-SEMANTICS-PLAN-001

- Smallest unblocker: none currently. The cross-cutting typed module export
  inventory landed under module ownership as
  `JN-MODULE-TYPED-EXPORT-INVENTORY-001`; there is no separate abstraction
  child to promote.
- Decision needed: none for that child. Preserve current non-aliased class
  capability imports, alias-hidden capabilities, class-attached impl payloads,
  and non-transitive module boundaries.
- Recommended default: keep the landed typed inventory behavior. Keep
  user-visible dictionaries, dictionary optimization, default methods,
  superclasses, new bundled method families, alias-qualified classes,
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
  overlap/orphan behavior, alias-qualified classes, re-exports, or any method
  visibility expansion beyond the landed typed inventory contract.

### JN-USER-DEFINED-OPERATORS-PLAN-001

- Smallest unblocker: none currently promotion-ready after custom
  associativity landed.
- Decision needed: accepted on `2026-06-30`: plan operator-specific type
  signatures, custom precedence, and custom associativity as separate child
  rows. Operator signatures, custom precedence, and custom associativity have
  landed; no later operator child has an accepted executable contract.
- Recommended default: keep Stage 2 fixed-tier parsing, same-source `(op) =
  <expr>.` execution, adjacent operator signatures, and custom numeric
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
  polymorphism, primitive mixed-width or implicit promotion, or any `jazz-hs`/
  `jazz2` work.

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
  landed as `JN-MODULE-NAMESPACE-AWARE-EXPORT-001`.
- Decision needed: none. Optional namespace prefixes, bare-selector
  compatibility, omitted-list export-all, `()` export-nothing, local/public
  inventory separation, and no re-exports are implemented.
- Recommended default: preserve the completed namespace-aware export contract
  and `E4007`-`E4015` diagnostics until a separate source-backed module behavior
  contract is accepted.
- Candidate child: none currently.
- Target paths: not set until a separate module behavior contract is accepted.
- Verification: focused `ModuleImportParserSpec.hs`, `ParserFoundationSpec.hs`,
  `OperatorFixitySpec.hs`, `ModuleExportsSpec.hs`,
  `ModuleResolutionSpec.hs`, `ModulePipelineContractSpec.hs`, and `LoaderSpec.hs`;
  `cabal build --project-dir=jazz-next all`;
  `cabal test --project-dir=jazz-next all --test-show-details=failures`;
  `bash scripts/check-execution-queue.sh`; `bash scripts/check-docs.sh`;
  `git diff --check`.
- Landed evidence: `jazz-next/src/JazzNext/Compiler/ModuleExports.hs` owns the
  typed inventory and structured selectors; module headers accept exact
  `value`, `constructor`, `type`, and `class` prefixes plus bare compatibility;
  `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs` separates local and public
  inventories; compiler imports and runtime publication consume the public
  inventory; focused and full verification passed on `2026-07-10`.
- Not in scope: re-exports, wildcard or constructor-group shorthand, body-level
  export declarations, visibility modifiers, cross-module operators,
  alias-qualified classes, separate impl imports, orphan/overlap policy,
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
