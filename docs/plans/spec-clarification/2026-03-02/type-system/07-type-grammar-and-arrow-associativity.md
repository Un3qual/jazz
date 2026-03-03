# Type Grammar + Arrow Associativity Clarification Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: use `executing-plans` to execute this clarification plan with explicit decision checkpoints.
>
> **Scope guard:** This plan only covers unresolved type-grammar semantics not already covered by spec-cleanup items #1-#6.

**Goal:** Resolve ambiguous type-grammar semantics by defining canonical rules for function-type arrow associativity, constrained type signatures, and type grammar normalization.

**Architecture:** Use a docs-first clarification flow with decision gates, then enforce with parser/analyzer test expectations. Keep implementation choices open where evidence is incomplete, but force explicit rationale records before code changes.

**Tech Stack:** Haskell (`jazz-hs` parser/analyzer/tests via `stack`), Markdown spec docs, pinned Nix shell commands for reproducibility.

---

## Plan Progress

- [x] Verified unresolved type-grammar ambiguity in current docs/tests/parser.
- [x] Verified non-overlap boundaries with spec-cleanup #1-#6 artifacts.
- [ ] Phase 0 complete: baseline evidence lock + unresolved question matrix.
- [ ] Phase 1 complete: arrow associativity decision and migration policy.
- [ ] Phase 2 complete: constrained type-signature semantic contract.
- [ ] Phase 3 complete: canonical type grammar and normalization rules.
- [ ] Phase 4 complete: parser/analyzer conformance checks and closure notes.

## Verification Evidence (Still Unclear)

- `docs/jazz-language-state.md:175`
  Observation: explicitly says function arrows are currently parsed left-associatively and calls it "almost certainly accidental," which confirms semantic intent is unresolved.
- `jazz-hs/src/Parser/Lang.hs:95`
  Observation: `lambdaTypeP` uses `foldl TLambda`, hard-coding left-associative parse trees for chained `->`.
- `jazz-hs/test/ParserSpec.hs:491`
  Observation: parser tests currently assert left-associative output for `Integer -> Integer -> Integer`, codifying current behavior but not language intent.
- `jazz-hs/src/Parser/Lang.hs:124`
  Observation: constrained signatures are parsed as `@{...}:` into `[Type]`, but grammar/meaning rules (ordering, duplicates, scope, normalization) are not specified.
- `jazz-hs/src/Analyzer/TypeInference.hs:169`
  Observation: `ETypeSignature ... tcConstraints ty` ignores `tcConstraints`, so constraint syntax exists without defined semantic effect.
- `jazz-hs/test/Analyzer/TypeInferenceSpec.hs:158`
  Observation: type-signature inference tests are commented out as "Currently broken," leaving constrained signature behavior unverified.
- `docs/jazz-language-state.md:173`
  Observation: only gives one constrained signature example and no canonical grammar/precedence/normalization section.
- `docs`
  Observation: only one language-state doc exists; there is no dedicated canonical type grammar spec doc to resolve parser-vs-intent conflicts.

## Non-Overlap With Spec-Cleanup #1-#6

- [x] Item #1 (`01-authoritative-syntax.md`) defines high-level surface syntax, but does not resolve `->` associativity semantics in type grammar.
- [x] Item #2 (`02-map-filter-order.md`) is collection API argument order only.
- [x] Item #3 (`03-purity-bang-semantics.md`) explicitly lists full effect typing in function types as non-goal for now.
- [x] Item #4 (`04-trait-vs-class-keyword.md`) marks constraint syntax redesign as out-of-scope.
- [x] Item #5 (`05-readme-implemented-vs-planned.md`) concerns documentation status separation, not type grammar semantics.
- [x] Item #6 (`06-parse-only-features-resolution.md`) addresses parser/analyzer/codegen completeness, but not canonical semantics for arrow associativity or constrained-signature meaning.

## Clarification Targets

- [ ] Function type arrow associativity and parenthesization rules are canonical and explicit.
- [ ] Constrained type-signature semantics are defined beyond syntax shape.
- [ ] Canonical type grammar exists as normative source (with precedence/associativity).
- [ ] Parser/analyzer/tests are mapped to canonical semantics with explicit compatibility policy.

## Phase 0: Baseline Lock And Open-Question Matrix

- [ ] Create a dedicated question matrix with one row per unresolved semantic.
- [ ] Capture current parser/test/analyzer behavior for each row using exact path/line evidence.
- [ ] Mark each row as `SPEC_DECISION_REQUIRED`, `IMPLEMENTATION_DRIFT`, or `BOTH`.
- [ ] Freeze this matrix before semantic decisions to avoid hindsight edits.

Deliverable file:
- `docs/spec/type-system/open-questions.md`

### Commit Checkpoint 0

Suggested commit message:
`docs(spec): baseline unresolved type-grammar questions`

Exact `git add` targets:

```bash
git add docs/plans/spec-clarification/2026-03-02/type-system/07-type-grammar-and-arrow-associativity.md \
  docs/spec/type-system/open-questions.md
```

## Phase 1: Function Arrow Associativity Decision

Decision gate (must be explicitly recorded):
- [ ] Option A: canonicalize `->` as right-associative (`a -> b -> c` means `a -> (b -> c)`).
- [ ] Option B: keep left-associative as language-specific choice, with explicit rationale and migration notes.
- [ ] Decide parenthesization requirements for mixed/explicit forms:
  - `(a -> b) -> c`
  - `a -> (b -> c)`
  - nested inside constructor/list/tuple types.
- [ ] Decide compatibility strategy:
  - hard switch (parser behavior changes immediately), or
  - compatibility window (accept legacy parse shape with warning/deprecation timeline).

Execution tasks:
- [ ] Add parser tests that encode the chosen canonical AST shape for chained arrows.
- [ ] Add parser tests for explicit parenthesized override forms.
- [ ] Add at least one negative or ambiguity test that documents rejected grammar.

Primary files:
- `jazz-hs/test/ParserSpec.hs`
- `jazz-hs/src/Parser/Lang.hs`
- `docs/spec/type-system/type-grammar.md`
- `docs/jazz-language-state.md`

### Commit Checkpoint 1

Suggested commit message:
`docs(spec): decide function type arrow associativity and compatibility policy`

Exact `git add` targets:

```bash
git add docs/spec/type-system/type-grammar.md \
  docs/jazz-language-state.md \
  jazz-hs/test/ParserSpec.hs \
  jazz-hs/src/Parser/Lang.hs
```

## Phase 2: Constrained Type-Signature Semantic Contract

Decision gates (semantic, not just parse shape):
- [ ] Define what constraints in `x :: @{...}: t` mean at type-check time.
- [ ] Define whether duplicate constraints are legal, normalized, or rejected.
- [ ] Define whether order in `@{C1, C2}` is semantically irrelevant and normalized canonically.
- [ ] Define variable-binding scope rules for constrained variables (`a`, `b`, etc.).
- [ ] Define relationship between signature constraints and inferred constraints.
- [ ] Decide representation contract (`[Type]` compatibility vs introducing dedicated constraint AST type).

Execution tasks:
- [ ] Add analyzer tests for constrained signature acceptance/rejection and intended behavior.
- [ ] Un-comment/replace currently broken type-signature inference tests with explicit expected outcomes.
- [ ] Document at least one invalid constrained signature form with expected diagnostic.

Primary files:
- `jazz-hs/test/Analyzer/TypeInferenceSpec.hs`
- `jazz-hs/src/Analyzer/TypeInference.hs`
- `jazz-hs/src/AST.hs`
- `docs/spec/type-system/type-signatures.md`

### Commit Checkpoint 2

Suggested commit message:
`spec(type): define constrained signature semantics and analyzer expectations`

Exact `git add` targets:

```bash
git add docs/spec/type-system/type-signatures.md \
  jazz-hs/src/AST.hs \
  jazz-hs/src/Analyzer/TypeInference.hs \
  jazz-hs/test/Analyzer/TypeInferenceSpec.hs \
  jazz-hs/test/ParserSpec.hs
```

## Phase 3: Canonical Type Grammar And Normalization Rules

- [ ] Publish normative grammar (EBNF-style) for:
  - simple types, constructor application, tuple/list types
  - function types with precedence/associativity
  - constrained signatures syntax and separators
- [ ] Define canonical formatting/normalization rules used by docs/examples/tests:
  - parenthesis minimization rules
  - canonical constraint ordering rule (if chosen)
  - canonical spacing and separator style.
- [ ] Add a parser-to-canonical-shape verification checklist (parser tests or golden cases).
- [ ] Add explicit non-goals to avoid conflating this work with effect typing and item #6 feature-completeness decisions.

Primary files:
- `docs/spec/type-system/type-grammar.md`
- `docs/spec/type-system/type-signatures.md`
- `docs/jazz-language-state.md`

### Commit Checkpoint 3

Suggested commit message:
`docs(spec): canonicalize Jazz type grammar and signature normalization`

Exact `git add` targets:

```bash
git add docs/spec/type-system/type-grammar.md \
  docs/spec/type-system/type-signatures.md \
  docs/jazz-language-state.md
```

## Phase 4: Reproducible Verification And Closure

- [ ] Run parser/analyzer tests that cover arrow associativity and constrained signatures.
- [ ] Run repository searches to ensure no contradictory type-grammar examples remain.
- [ ] Confirm spec-cleanup #1-#6 references remain accurate and this clarification scope stays orthogonal.
- [ ] Record closure note with unresolved follow-up items (if any).

### Reproducible Verification Commands (Repo Root Flake)

```bash
nix flake check
nix develop . -c bash -lc '
  set -euo pipefail
  rg -n -- "a -> b -> c|@\\{|type grammar|left-associatively|right-associatively" \
    docs/jazz-language-state.md docs/spec/type-system jazz-hs/src/Parser/Lang.hs jazz-hs/test/ParserSpec.hs
  cd jazz-hs
  stack test --ta "--match Tests of types"
  stack test --ta "--match Type Inference"
'
```

Expected verification outcomes:
- parser tests encode only the chosen canonical associativity behavior.
- analyzer tests cover constrained signature semantics (including at least one invalid case).
- docs and spec files agree on one canonical type grammar.

### Commit Checkpoint 4

Suggested commit message:
`chore(spec): verify and close type-grammar clarification plan`

Exact `git add` targets:

```bash
git add docs/spec/type-system/type-grammar.md \
  docs/spec/type-system/type-signatures.md \
  docs/jazz-language-state.md \
  jazz-hs/src/Parser/Lang.hs \
  jazz-hs/src/Analyzer/TypeInference.hs \
  jazz-hs/test/ParserSpec.hs \
  jazz-hs/test/Analyzer/TypeInferenceSpec.hs
```

## Nix Environment Considerations

- [x] Repo-level `flake.nix` is available and is the canonical reproducibility entrypoint.
- [x] Verification commands should run via `nix develop . -c ...` from repo root.
- [ ] Keep plan snippets aligned with root-flake toolchain updates over time.

## Suggested Follow-On Sub-Plans (If Scope Splits)

- `docs/plans/spec-clarification/2026-03-02/type-system/07a-arrow-associativity-decision.md`
- `docs/plans/spec-clarification/2026-03-02/type-system/07b-constrained-signature-semantics.md`
- `docs/plans/spec-clarification/2026-03-02/type-system/07c-type-grammar-canonicalization.md`

Use these splits only if one track blocks others or requires independent review/approval cadence.

## Completion Criteria

- [ ] Canonical function-arrow associativity is explicit and test-backed.
- [ ] Constrained type-signature semantics are explicit and analyzer-backed.
- [ ] Canonical type grammar doc exists and is referenced from language-state docs.
- [ ] Verification commands are reproducible in pinned Nix shell.
- [ ] Scope boundaries with cleanup items #1-#6 remain explicit.

## Completed In This Task

- [x] Analyzed old code + current spec/plans for unresolved type-grammar semantics.
- [x] Authored a phased clarification plan with verification evidence and commit checkpoints.
- [x] Added Nix reproducibility guidance and suggested sub-plan split paths.
