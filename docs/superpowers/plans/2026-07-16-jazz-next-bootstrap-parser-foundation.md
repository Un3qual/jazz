---
id: JN-BOOTSTRAP-JAZZ-PARSER-FOUNDATION-001
status: done
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-JAZZ-PARSER-DESIGN-001
last_verified: 2026-07-16
completed_on: 2026-07-16
plan_section: "Implementation Batch: Parser Contract and Kernel"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-12-jazz-next-bootstrap-jazz-parser-design.md
  - docs/superpowers/plans/2026-07-16-jazz-next-bootstrap-parser-foundation.md
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Expression.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Signature.hs
  - jazz-next/src/JazzNext/Compiler/Parser/TokenParser.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs
  - jazz-next/jazz-next.cabal
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next canonical-parser-comparison-spec parser-core-spec token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec repository-audit-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Define the complete ordinary Jazz parser surface/result/failure schema, preserve structured stage-0 parser failures behind compatible diagnostics, add a total test-only Haskell normalization adapter, and implement a generic compiler-local Jazz parser kernel with deterministic consumption, rollback, error selection, progress, and large-input behavior without adding grammar."
---

# Jazz-Next Bootstrap Parser Foundation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Establish the permanent parser contract and reusable compiler-local
parser kernel required before any Jazz-authored grammar is added.

**Architecture:** Stage 0 gains structured parser failures while retaining its
current diagnostic-returning public API. Ordinary Jazz ADTs own the complete
surface/result/failure schema, a test-only Haskell adapter normalizes stage-0
values into that schema, and a generic Jazz parser kernel operates over an
immutable remaining-token cursor with explicit consumption. The child ends
before program, expression, declaration, pattern, signature, or operator
grammar begins.

**Tech Stack:** GHC 9.14.1, Haskell 2010 with existing package extensions,
Jazz `.jz` modules, Megaparsec-backed stage-0 parsing, the shared Jazz
interpreter and module graph, the generic runtime-value renderer, Cabal test
components, and Nix-pinned repository verification.

## Global Constraints

- Modify compiler implementation only under `jazz-next/`; `jazz-hs/` and
  `jazz2/` remain read-only references.
- Keep the parser fail-fast. Return either one complete result or one
  structured failure; do not add recovery, partial ASTs, or multiple errors.
- Compare the complete parser-owned surface AST before lowering. Do not create
  a second canonical AST family or compare lowered core as parser evidence.
- Keep the existing diagnostic-returning Haskell parser entry points
  source-compatible and preserve their user-facing `E0001` summaries and
  primary spans.
- Do not infer structured failure reasons by inspecting rendered diagnostic
  text.
- Keep the primary hosted parser boundary at canonical lexer tokens. The later
  source façade must preserve separate lexical-failure and parser-failure
  outcomes.
- Store integer literals as normalized decimal text in the Jazz schema. Store
  fractional literals as normalized whole-number text, exact fractional digit
  text with scale preserved, and the optional width suffix.
- Keep `ParserCore` generic over token, error, and result types and independent
  of Jazz token, AST, grammar-context, and diagnostic types.
- Use direct, approachable parser API names; do not introduce category-theory
  vocabulary merely to mirror a Haskell library.
- Add no parser-specific builtin, Haskell parser callback, mutable host token
  buffer, custom serializer, bytecode, VM, lowered IR, LLVM value, object
  production, linking, or native-runtime implementation.
- Keep every checked-in `.jz` module at exactly two spaces per indentation
  level and validate it through `repository-audit-spec`.
- Use `MultilineStrings` for fixed Jazz programs embedded in Haskell tests.
  Retain explicit construction only for generated/injected fragments or tests
  whose subject is whitespace, spans, or line endings.
- Use behavior-first tests. Source searches may be used once during review to
  confirm boundaries but must not become implementation-spelling tests.
- Commit after each independently reviewable task.

---

## File Ownership Map

| File | Responsibility |
| --- | --- |
| `jazz-next/src/JazzNext/Compiler/Parser/Failure.hs` | New production owner for structured stage-0 parser failure values, reason payloads, and the single diagnostic renderer. |
| `jazz-next/src/JazzNext/Compiler/Parser/TokenParser.hs` | Detailed Megaparsec runner, token expectations, progress-aware stage-0 failures, and compatibility runners. |
| `jazz-next/src/JazzNext/Compiler/Parser.hs` | Detailed complete-program entry point plus unchanged diagnostic-returning façade. |
| `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs` | Structured declaration/module/signature/operator-declaration failure construction; no grammar change. |
| `jazz-next/src/JazzNext/Compiler/Parser/Expression.hs` | Structured expression/control-flow failure construction; no grammar change. |
| `jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs` | Structured pattern failure construction; no grammar change. |
| `jazz-next/src/JazzNext/Compiler/Parser/Signature.hs` | Detailed signature-parser path plus unchanged compatibility functions. |
| `jazz-next/src/JazzNext/Compiler/Parser/AST.hs` | Source of truth whose complete semantic constructors are mirrored by the ordinary Jazz surface schema. |
| `jazz-next/jazz/compiler/ParserTypes.jz` | Ordinary Jazz-owned surface AST, numeric source values, parser reasons, token/source results, and future façade result ADTs. |
| `jazz-next/jazz/compiler/ParserCore.jz` | Generic immutable parser cursor, replies, consumption semantics, and combinators; no Jazz grammar. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalValue.hs` | Shared test-only logical path, span, constructor, and runtime-value encoding helpers used by lexer and parser adapters. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparison.hs` | Total test-only stage-0 surface/failure normalization and deterministic rendering. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs` | Schema completeness, numeric fidelity, structured failure, adapter, and deterministic stage-0 corpus tests. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/ParserCoreSpec.hs` | Real-module-graph behavioral tests for the generic Jazz parser kernel. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs` | Reuses shared canonical-value helpers while preserving existing lexer comparison exports and behavior. |
| `jazz-next/jazz-next.cabal` | Registers new production/test modules, checked-in compiler sources, and focused Cabal suites. |
| Active docs listed in frontmatter | Records design acceptance, current promotion, completion evidence, and the later unpromoted expression child. |

New files are listed here even though the live queue can name only existing
paths before implementation. The queue/frontmatter target list therefore names
the existing owners that the child will change; this map is the complete file
creation contract.

## Stable Interfaces Between Tasks

The exact constructor layout belongs in implementation, but these semantic
interfaces are fixed so later tasks do not invent parallel representations:

- Stage 0 exposes a structured `ParserFailure` and `ParserFailureReason`, a
  single `parserFailureDiagnostic` conversion, and detailed token/program
  runners. Existing `Either Diagnostic` entry points remain wrappers.
- `ParserTypes.jz` owns one ordinary surface tree and one parser-result family.
  Parser success and failure both retain the normalized logical source path.
  The later source façade has distinct success, lexical-failure, and
  parser-failure cases.
- `CanonicalParserComparison` accepts the logical path plus a detailed stage-0
  result, produces the same ordinary value schema as `ParserTypes.jz`, and
  renders only through `renderRuntimeValue`.
- `ParserCore.jz` exports the parser/cursor/reply types and the operations
  `parserRun`, `parserSucceed`, `parserFail`, `parserTransform`,
  `parserAndThen`, `parserKeepLeft`, `parserKeepRight`, `parserChoice`,
  `parserAttempt`, `parserLookAhead`, `parserOptional`, `parserMany`,
  `parserOneOrMore`, `parserSeparatedBy`, `parserPeek`, and `parserTakeIf`.
  Names may receive a small Jazz-required spelling correction, but their
  responsibilities must not be merged or silently omitted.
- The kernel reports a zero-progress repetition failure distinctly from a
  caller-supplied token/grammar rejection. Later grammar code must be able to
  turn that internal failure into a structured parser reason without parsing
  text.

## Implementation Batch: Parser Contract and Kernel

### Task 1: Preserve structured stage-0 parser failures

**Files:**

- Create: `jazz-next/src/JazzNext/Compiler/Parser/Failure.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/TokenParser.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Expression.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Signature.hs`
- Modify: `jazz-next/jazz-next.cabal`
- Test: existing parser suites under `jazz-next/test/JazzNext/Compiler/Parser/`

**Produces:** A structured failure path for every current stage-0 parser
rejection, with the legacy diagnostic API implemented only as an adapter.

- [x] **Step 1: Add RED tests for detailed failures and compatibility.**

  Extend the smallest existing owner suite for each failure family. Assert
  reason identity and payload, optional span, and deterministic farthest
  failure selection for detailed entry points. For the same inputs, assert that
  the compatibility entry points retain their current error code, summary, and
  primary span. Include end-of-input, expected/found token, declarations,
  signatures, modules/imports/exports, expressions, patterns, control flow,
  numeric overflow, and operator-boundary cases.

- [x] **Step 2: Run the focused parser suites RED.**

  Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next token-parser-spec declaration-parser-spec expression-parser-spec parser-foundation-spec adt-pattern-parser-spec pattern-parser-spec operator-invalid-syntax-spec --test-show-details=failures
  ```

  Expected: the newly added detailed-failure tests fail because the structured
  production type and detailed entry points do not exist yet; unchanged legacy
  assertions continue to pass.

- [x] **Step 3: Introduce one structured failure model and renderer.**

  Inventory every current `E0001` parser construction site before editing it.
  Group equivalent expectations under shared reasons and give genuinely
  grammar-specific failures typed payloads rather than opaque prose. Move all
  user-facing wording into `parserFailureDiagnostic`. Make Megaparsec carry the
  structured value, and make token helpers build expected/found data directly
  from the encountered token or end of input.

  Convert the direct `Either Diagnostic` helpers in declaration and signature
  parsing to detailed internal paths, retaining compatibility wrappers wherever
  another current module imports the diagnostic-returning signature. Do not
  alter token consumption, choice order, grammar dispatch, or accepted syntax.

- [x] **Step 4: Run all parser suites GREEN and inspect behavior parity.**

  Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next token-parser-spec declaration-parser-spec expression-parser-spec if-expression-parser-spec lambda-parser-spec module-import-parser-spec operator-fixity-spec operator-invalid-syntax-spec operator-section-spec parser-foundation-spec pattern-parser-spec adt-pattern-parser-spec --test-show-details=failures
  ```

  Expected: every parser suite passes; existing accepted ASTs and rendered
  diagnostics are unchanged, while detailed tests observe typed reasons.

- [x] **Step 5: Commit the structured stage-0 failure path.**

  Stage only the production parser modules and their focused parser tests.
  Commit as `refactor: structure parser failures`.

### Task 2: Define the ordinary parser schema and total Haskell adapter

**Files:**

- Create: `jazz-next/jazz/compiler/ParserTypes.jz`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalValue.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparison.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Consumes:** The structured stage-0 result from Task 1 and the complete surface
tree in `Parser.AST`.

**Produces:** One Jazz-owned schema and a total Haskell normalization path that
future grammar slices compare without using JSON or presentation prose.

- [x] **Step 1: Add RED schema and adapter coverage.**

  Register `canonical-parser-comparison-spec`. Add table-driven cases that
  cover every surface literal, numeric width, pattern, lambda parameter,
  expression, signature payload/type/token, constraint, class/impl/data
  payload, statement, module export selector, parser reason, optional span, and
  source-façade result case. Require the Jazz module to load through the real
  compiler source root and construct representative values that render exactly
  like the Haskell mirror.

  Include source-exact cases for an integer beyond `Int64`, fractional leading
  and trailing zeroes, preserved fractional scale, all width suffixes, and a
  Float64 overflow rejection. Include a coverage check that fails when a new
  Haskell surface or parser-reason constructor lacks an adapter branch. Run the
  fixed 333-case manifest through lexing plus the detailed stage-0 parser,
  distinguish lexical failures from parser failures, canonicalize every result
  twice in manifest order, and require byte-identical rendering.

- [x] **Step 2: Run canonical comparison tests RED.**

  Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next canonical-parser-comparison-spec canonical-lexer-comparison-spec --test-show-details=failures
  ```

  Expected: the parser comparison component cannot build until the new schema,
  shared canonical helpers, and total adapter exist.

- [x] **Step 3: Implement the schema and normalization boundary.**

  Mirror semantic distinctions from `Parser.AST` rather than Haskell record
  layout or constructor prefixes. Keep identifiers, module components,
  operators, signature tokens, and export selectors distinct. Reuse
  `LexerTypes` for canonical source paths, spans, tokens, and lexical failures.
  Represent optional values with `Maybe` and semantically nonempty values with
  `NonEmpty` where the active surface tree guarantees them.

  Extract only genuinely shared logical-path/span/runtime-constructor helpers
  from the lexer adapter into `CanonicalValue`; keep existing lexer adapter
  exports source-compatible. Normalize Haskell integers and fractional source
  metadata directly, never through `Double` rendering. Build ordinary
  `RuntimeValue` constructors and render them with the existing generic
  renderer.

- [x] **Step 4: Prove schema completeness and lexer non-regression GREEN.**

  Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next canonical-parser-comparison-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec repository-audit-spec --test-show-details=failures
  ```

  Expected: parser schema/adapter tests pass, both existing lexer comparison
  suites retain byte-identical output, and repository source-layout/formatting
  checks accept `ParserTypes.jz`.

- [x] **Step 5: Commit the canonical parser contract.**

  Stage the Jazz schema, shared/test adapters, focused specs, and Cabal
  registration. Commit as `feat: define canonical parser contract`.

### Task 3: Implement the generic compiler-local Jazz parser kernel

**Files:**

- Create: `jazz-next/jazz/compiler/ParserCore.jz`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/ParserCoreSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Consumes:** Ordinary `List`, `Maybe`, `Result`, and `NonEmpty` APIs. It does
not consume `LexerTypes`, `ParserTypes`, or any grammar module.

**Produces:** A generic parser abstraction whose public operations and
consumption rules are stable inputs to the expression-foundation child.

- [x] **Step 1: Add RED kernel behavior tests through the real module graph.**

  Register `parser-core-spec` and construct small Jazz test programs that use
  the public operations named in the stable-interface section. Cover initial
  cursor state, one-token consumption, unchanged cursor on rejection,
  transform/sequencing, keep-left/right, committed choice, explicit rollback,
  lookahead, optional parsing, repetition, one-or-more, separated sequences,
  peek, and predicate-based token consumption.

  Assert exact reply values rather than only successful execution. Include
  alternatives that fail at different offsets and at equal offsets so the
  farthest-failure and declaration-order tie rules are observable.

- [x] **Step 2: Run the kernel suite RED.**

  Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next parser-core-spec --test-show-details=failures
  ```

  Expected: the suite cannot load `ParserCore` until the compiler-local module
  and its public types/operations exist.

- [x] **Step 3: Implement the smallest kernel that satisfies the contract.**

  Retain the remaining token list and monotonic offset in an immutable cursor.
  Make every failure carry its offset and consumed/unconsumed status. Choice
  retries only an unconsumed failure; `parserAttempt` alone makes a consumed
  failure retryable. Lookahead restores the original cursor and consumption
  state. Farthest failure wins; equal offsets keep the earlier declared
  alternative.

  Implement repetition with a tail-recursive accumulator and explicitly reject
  success without offset progress. Keep parser context out of the kernel. Do
  not add a public stdlib parser module or any raw runtime bridge.

- [x] **Step 4: Run focused kernel and repository suites GREEN.**

  Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next parser-core-spec loader-spec repository-audit-spec --test-show-details=failures
  ```

  Expected: all kernel cases pass, checked-in compiler modules load through the
  real source boundary, and source layering/formatting remains valid.

- [x] **Step 5: Commit the generic parser kernel.**

  Stage `ParserCore.jz`, its focused spec, and Cabal registration. Commit as
  `feat: add Jazz parser core`.

### Task 4: Prove deterministic large-input behavior and architectural limits

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/ParserCoreSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs`
- Modify: `jazz-next/jazz-next.cabal` only if suite registration needs shared
  test modules not already named.

**Produces:** Review evidence that the foundation is safe for grammar work and
does not hide a quadratic cursor or host-stack dependency.

- [x] **Step 1: Add RED large-input and determinism cases.**

  Add a 20,000-token successful traversal that verifies final offset and empty
  remainder, a long failing traversal whose selected failure is at the exact
  farthest offset, and a zero-progress repetition that terminates with the
  distinct internal problem. Render representative success and failure
  batches twice and require byte-identical output.

  Keep physical timings as review evidence, not fixed thresholds. A generous
  timeout may guard against nontermination or host-stack failure, but the test
  must not enforce a machine-specific performance baseline.

- [x] **Step 2: Run the large cases RED against any incomplete kernel.**

  Run `parser-core-spec` alone and verify that the new cases catch any
  non-tail-recursive repetition, repeated indexing of the original list,
  incorrect failure-offset selection, or zero-progress loop.

- [x] **Step 3: Correct the underlying traversal behavior.**

  Fix cursor or repetition ownership inside `ParserCore.jz`; do not add a
  special large-input path, test-only fast path, mutable token buffer, or host
  primitive. If the original implementation already passes, make no production
  edit merely to create activity.

- [x] **Step 4: Run focused, development-warning, and full verification.**

  Run every command from the plan frontmatter in order. Record same-machine
  elapsed-time and allocation observations for the focused kernel suite in the
  PR evidence, but do not commit generated benchmark/profile output. Inspect
  the two parser Jazz modules once to confirm they import no Haskell callback
  or kernel-only host symbol; rely on module-graph behavior and repository
  audits for permanent regression coverage.

- [x] **Step 5: Commit the large-input evidence.**

  Stage only behavioral tests and any necessary root-cause kernel correction.
  Commit as `test: prove parser core scale and determinism`.

### Task 5: Close the foundation child and curate the expression child

**Files:**

- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/done-archive.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md`
- Modify: `docs/superpowers/specs/2026-07-12-jazz-next-bootstrap-jazz-parser-design.md`
- Modify: `docs/superpowers/plans/2026-07-16-jazz-next-bootstrap-parser-foundation.md`

**Produces:** Historical closure evidence and one named curation target for the
expression-foundation child, without prematurely promoting grammar work.

- [x] **Step 1: Re-run all completion gates from a clean working tree.**

  Require the focused suites, `-fdevelopment` build, full suite, queue/docs
  validators, and `git diff --check` to pass at the implementation head. Do not
  close the row using earlier task-level results.

- [x] **Step 2: Record exact completion evidence.**

  Change this plan to `status: done`, set `completed_on` and `last_verified` to
  the actual completion date, and check completed steps. Move
  `JN-BOOTSTRAP-JAZZ-PARSER-FOUNDATION-001` from `Ready Now` into the done
  archive with the landed behavior and commands, not planned claims.

- [x] **Step 3: Curate but do not promote the expression foundation.**

  Update the bootstrap blocker so the next smallest child is the design's
  expression foundation: program/block sequencing, literals, names,
  application, lists, tuples, unit, ordinary bindings, and expression
  statements over an explicit named fixture family. Put it in `Next Curation
  Target` only. It needs its own implementation plan before entering `Ready
  Now`; later type/declaration/module, control-flow/pattern, and operator/full
  parity children remain ordered and unpromoted.

- [x] **Step 4: Commit the closeout.**

  Stage only active docs and this plan. Commit as
  `docs: close Jazz parser foundation child`.

## Completion Evidence

Completed on `2026-07-16`. The implementation preserves structured stage-0
failures behind the existing diagnostic API, defines and executes the complete
ordinary Jazz parser contract, and adds the grammar-neutral compiler-local
parser kernel without introducing grammar or backend work.

- The focused canonical parser, parser core, token parser, canonical lexer,
  Jazz lexer parity, and repository audit suites passed.
- `cabal build --project-dir=jazz-next -fdevelopment all` passed with warnings
  promoted to errors, including exhaustive Haskell adapter matches.
- `cabal test --project-dir=jazz-next all --test-show-details=failures` passed.
- The 333-case source façade renders deterministically with distinct lexical,
  parser, and success outcomes.
- The kernel completes the 20,000-token success floor at offset 20,000, selects
  the long farthest failure at offset 20,000, and rejects zero-progress
  repetition distinctly.
- A same-machine focused run with RTS statistics completed in 11.283 seconds
  elapsed inside the test executable, allocated 45,457,808,376 bytes across
  repeated module compilation/evaluation, and retained 3,938,232 bytes at peak.
  These are review observations, not a pass/fail baseline.
- Queue/docs validators and `git diff --check` passed at closeout.

## Acceptance Checklist

- Every current stage-0 parser rejection has a structured reason before
  diagnostic rendering; no adapter parses diagnostic prose.
- Existing stage-0 parser APIs, accepted ASTs, error codes, summaries, and
  primary spans remain compatible.
- The ordinary Jazz schema covers the full surface AST and all structured
  failure reasons, with source-exact numeric normalization.
- Haskell and Jazz schema values render through the generic runtime-value
  renderer with no JSON or custom serializer.
- The parser kernel is generic, compiler-local, predictive, explicitly
  consumption-aware, deterministic, progress-safe, and tail-recursive.
- Large successful/failing traversals complete without host-stack growth or
  whole-remainder rescanning.
- No Jazz grammar, lowering, canonical core, bytecode, LLVM, object, linker, or
  native-runtime work enters this child.
- The implementation ends with the expression foundation curated but not
  promoted without its own plan.
