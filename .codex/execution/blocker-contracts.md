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
  `JN-BOOTSTRAP-JAZZ-PARSER-FOUNDATION-001` then preserved structured stage-0
  parser failures behind compatible diagnostics, added the complete ordinary
  `ParserTypes` contract and total Haskell comparison adapter, and implemented
  the generic compiler-local `ParserCore` with deterministic consumption,
  rollback, farthest-failure selection, progress checks, and 20,000-token
  traversal evidence without adding grammar.
  `JN-BOOTSTRAP-JAZZ-PARSER-EXPRESSION-FOUNDATION-001` added the compiler-local
  `ParserToken`, `ParserExpression`, `ParserProgram`, and `Parser` stack for
  literals, names, application, lists, tuples, unit, ordinary statements,
  recursive blocks, complete programs, and distinct token/source façades. Its
  explicit 43-case family matches stage 0 exactly and its isolated 512-binding
  scale suite produces deterministic runtime observations without host
  operations.
  `JN-BOOTSTRAP-JAZZ-PARSER-TYPES-DECLARATIONS-MODULES-001` added shared
  signature types and explicit type application, immutable scoped alias
  context, data/class/impl declarations, module/import/export forms, exact
  101-case stage-0 parity through both façades, and deterministic
  513-statement mixed scale evidence. It preserved the fixed parser schema and
  left `ParserCore` and `ParserTypes` unchanged.
  `JN-BOOTSTRAP-JAZZ-PARSER-CONTROL-FLOW-PATTERNS-001` added independent
  compiler-local pattern grammar plus stop-aware lambdas, conditionals, cases,
  single case-arm guards, and nested control-flow bodies. Its fixed 75-case
  family matches complete stage-0 token/source results twice, both landed
  families remain exact, and all three 513-statement scale profiles are
  deterministic with zero host operations. `ParserCore`, `ParserTypes`,
  `ParserContext`, and the parser façades remain unchanged.
  `JN-BOOTSTRAP-JAZZ-PARSER-OPERATORS-FULL-PARITY-001` completed the fifth and
  final grammar child with centralized operator metadata, immutable
  source-order context, declarations, bindings, signatures, precedence,
  associativity, values, sections, and mixed control-flow composition. Its six
  52 / 101 / 75 / 55 / 26 / 56 case families assign all 365 fixtures exactly
  once and match complete token/source results twice. All four scale profiles
  are deterministic with zero host operations; the operator profile records
  49,040,140 transitions / 5,914,883 applications / 186,465 list cells / depth
  1,116.
  `JN-BOOTSTRAP-JAZZ-CORE-EXPRESSION-FOUNDATION-001` then defined the complete
  ordinary Jazz canonical-core schema, a checked structural stage-0 adapter,
  and the shared direct/composed parity harness. Its internal
  `lowerFoundationExpression` entry point exactly lowers literals, source and
  qualified names, operator values, collections, tuples, application,
  non-`$` binary nodes, both section forms, and ordinary blocks. Repeated
  direct and hosted-parser composition match stage 0 exactly, while every
  deferred or recursively unsupported tree returns `Nothing` without a fake
  core value or lowering diagnostic.
  `JN-BOOTSTRAP-JAZZ-CORE-CONTROL-FLOW-PATTERNS-001` then refactored the hosted
  lowerer around one shared profile-driven kernel and added every pattern,
  guarded case, conditional, nested control-flow, and multi-parameter or
  pattern-lambda rule. Its 18 direct and 14 composed positive fixtures match
  stage 0 twice; 12 root and nested later-child fixtures return only `Nothing`
  twice; and exact one-based generated parameter names are preserved for both
  binder and scrutinee.
  `JN-BOOTSTRAP-JAZZ-CORE-SIGNATURES-DECLARATIONS-OPERATORS-001` then added the
  ordered third lowering profile for every signature type, constraint,
  unsupported token, declaration payload, explicit type application, `$`
  application, and exact hidden operator-storage name. Its 20 direct and 16
  hosted-parser-composed positive fixtures match complete stage-0 values
  twice, while all 8 root and nested module/import fixtures return only
  `Nothing` twice.
  `JN-BOOTSTRAP-JAZZ-CORE-MODULES-CORPUS-CLOSURE-001` completed the fourth and
  final canonical-core profile with total expression lowering, exact
  module/import metadata and source qualification, structured `E4005`/`E4006`
  counterparts, and the single-call `Core.lowerCoreSource` facade. All 17
  direct module fixtures and 13 composed sources match stage 0 twice, and an
  audited ordered manifest covers all 196 accepted parser fixtures with exact
  repeated module results.
  `JN-BOOTSTRAP-LOWERED-IR-CONTRACT-FOUNDATION-001` then established matching
  Haskell/Jazz backend-neutral CFG schemas, complete stable validators, and a
  checked comparison adapter. Its exact 10-valid / 31-invalid manifest runs all
  41 programs through Jazz twice with complete ordered parity and keeps
  temporaries block-local.
- Accepted decision: [RFC 0004: Hosted canonical compiler](../../rfcs/accepted/0004-hosted-canonical-compiler.md)
  preserves the stage-0 parser/lowerer boundary, ordinary Jazz ADTs, pure
  lowering, and structural differential comparison before production cutover.
- Accepted decision: [RFC 0005: Typed-core elaboration](../../rfcs/accepted/0005-typed-core-elaboration.md)
  owns the separate typed tree and its implemented opt-in, single-pass,
  one-module scalar/direct-call profile while preserving canonical core and the
  interpreter boundary.
- Accepted decision: [RFC 0006: Lowered IR contract](../../rfcs/accepted/0006-lowered-ir-contract.md)
  owns the mirrored backend-neutral CFG, explicit calls and closures,
  representations, layout requests, structured validation, and canonical
  parity.
- Accepted decision: [RFC 0009: Typed-core closures and recursive callable groups](../../rfcs/accepted/0009-typed-core-closure-and-recursion.md)
  fixes callable-shape and binder-reference transport, unary closure staging,
  empty environments, deterministic capture identity, and the six-child
  delivery order without changing normal compile/run.
- Completed child: `JN-BOOTSTRAP-TYPED-CORE-EXPRESSION-DIRECT-CALL-001`
  completed on `2026-07-30`. It produces the verified opt-in single-pass
  scalar/direct-call typed-core profile and deterministic validated lowering;
  normal compile/run remains canonical-core/interpreter based.
- Completed child: `JN-BOOTSTRAP-TYPED-CORE-CLOSURE-CALL-FOUNDATION-001`
  completed on `2026-08-10`. The opt-in path now transports callable shape and
  binder references, closed named functions as values, recursively represented
  unary closure parameters and results, explicit empty environments, and unary
  higher-order closure calls while preserving every scalar/direct-call fixture.
  Normal compile/run remains canonical-core/interpreter based.
- Completed child: `JN-BOOTSTRAP-TYPED-CORE-SCALAR-BINDING-001` completed on
  `2026-08-12`. The opt-in producer now emits concrete scalar signatures and
  bindings in source order, later expressions carry exact scalar binder
  references, and entry lowering evaluates each initializer once and reuses
  its binder-indexed operand. At that milestone, managed values and
  function-body capture remained rejected. Normal compile/run remains
  canonical-core/interpreter based.
- Completed child: `JN-BOOTSTRAP-TYPED-CORE-LEXICAL-CAPTURE-001` completed on
  `2026-08-12`. The opt-in producer now resolves inline and nested lambda
  binders exactly; lowering emits deterministic lifted functions, immutable
  first-occurrence capture environments, and entry projections for scalar and
  closure-valued captures. Unsupported managed capture still fails closed.
  Normal compile/run remains canonical-core/interpreter based.
- Completed child: `JN-BOOTSTRAP-TYPED-CORE-CURRIED-APPLICATION-001` completed
  on `2026-08-12`. The opt-in producer and lowerer now preserve unary source
  staging for named functions, callable parameters, and inline lambdas;
  partial application returns the remaining closure, and additional arguments
  proceed only through callable intermediate results. Source non-callable
  oversaturation remains an ordinary diagnostic, and malformed typed artifacts
  fail validation. Normal compile/run remains canonical-core/interpreter-based.
- Completed child: `JN-BOOTSTRAP-TYPED-CORE-DIRECT-RECURSION-001` completed on
  `2026-08-12`. The opt-in producer now transports ordered direct recursive
  groups by exact binder identity, both validators enforce declared reachability,
  and the lowerer consumes those validated groups without rebuilding an SCC.
  Capture-free, non-escaping self and mutual recursion use the existing direct
  callable representation. Normal compile/run remains canonical-core/interpreter
  based.
- Smallest unblocker: curate
  `JN-BOOTSTRAP-TYPED-CORE-CLOSURE-RECURSION-001` from accepted RFC 0009.
- Decision needed: none for the semantic boundary. RFC 0009 fixes closure
  recursion as the sixth ordered child after direct recursion.
- Recommended default: validate the live G5 ownership matrix and create one
  matching closure-recursion implementation plan before promotion. Keep normal
  compile/run on canonical core and the reference interpreter.
- Candidate child: `JN-BOOTSTRAP-TYPED-CORE-CLOSURE-RECURSION-001`.
- Target paths: `src/Jazz/Compiler/RecursiveBindings.hs`;
  `test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs`;
  `src/Jazz/Compiler/TypeInference/Elaboration.hs`;
  `src/Jazz/Compiler/LoweredIR/Lower.hs`;
  `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`;
  `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`;
  `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`;
  `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`;
  `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`;
  `test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs`.
- Conditional target paths: typed-core mirrors only if group invariants change;
  `jazz.cabal` only for registration.
- Verification: `cabal test recursive-bindings-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1`;
  `bash scripts/check-execution-queue.sh`; `bash scripts/check-docs.sh`.
- Not in scope: re-promoting the completed closure-call, scalar-binding,
  lexical-capture, curried-application, or direct-recursion children;
  implementing closure recursion during curation; control flow; patterns;
  multi-module or import integration;
  non-closure managed values; runtime services; tail calls; LLVM emission;
  object generation; linking; native-runtime or ABI implementation; a public
  compiler embedding API; bytecode or a VM; or revival of removed legacy
  implementations.

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
