# Spec Clarification Item #11: ADT and Pattern-Matching Positioning

## Progress Tracker

- [x] Gather old-code + current-spec evidence for ADT/pattern ambiguity
- [x] Define decision gate (`core commitment` vs `optional scaffolding`)
- [x] Define spec/testing priority implications for each outcome
- [x] Define phased execution plan with commit checkpoints
- [x] Define Nix-based verification matrix
- [ ] Execute clarification and land chosen track

## Clarification Goal

Decide whether ADTs and pattern matching are:

1. **Core language commitments** (normative semantics with high implementation/test priority), or
2. **Optional scaffolding** (legacy/experimental parse surface with lower priority or planned removal).

Then encode that choice in spec ownership, test tiers, and cleanup sequencing.

## Verification Evidence (Ambiguity Is Real)

- `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md:8` and `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md:15`
  Observation: Top-level feature list claims both `ADTs` and `Pattern matching` as language features.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:320`
  Observation: Language-state doc says README strongly presents ADTs/pattern matching as features.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:398`
  Observation: Explicit unresolved question asks whether ADTs/pattern matching are central design vs inherited scaffolding.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/AST.hs:122`, `:129`, `:161`, `:169`
  Observation: AST includes `EData`, `ECase`, `FPPattern`, and constructor patterns (strong structural support).
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs:173-191`, `:265-279`
  Observation: Parser accepts `data`, `case`, lambda pattern params, and constructor patterns.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs:179-181`, `:226-229`, `:456-458`
  Observation: Inference/substitution error for unsupported expressions and explicitly for pattern params (`not implemented yet`).
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/ScopeAnalyzer.hs:103`, `:130`
  Observation: `EData` path is `undefined`; pattern-param scope analysis also `not implemented`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Javascript.hs:37`, `:43`, `:59`
  Observation: Tuple and pattern-param codegen fail with explicit `not implemented` errors; unsupported expressions (including `EData`/`ECase`) hit catch-all failure.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs:578-631`
  Observation: `case` parser tests exist but are commented out (feature present but not confidently enforced even at parser-test level).
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Spec.hs:13-45`
  Observation: Test runner emphasizes parser + simple inference + optimizer coverage; no end-to-end ADT/case execution track.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/ExamplePrograms/ComplexProgram.jz:1-17`
  Observation: Example programs exercise arithmetic/list/builtins and avoid ADT/case usage (practical surface excludes them).
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz:12-60`, `:80-120`
  Observation: Prelude models ADT/pattern-heavy intended design, but with dialect mismatch (`trait`/`impl` forms not aligned with active parser expectations), suggesting aspirational direction rather than current behavior.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz2/src/Jazz/AST.hs:145-157` and `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz2/src/Jazz/Parser/Lexer.hs:6`
  Observation: Rewrite AST retains constructor patterns and `ECase`, but parser/lexer are unfinished (`undefined`), so this does not resolve current semantic authority.

## Decision Gate (What Must Be Explicitly Chosen)

- [ ] **Positioning:** `CORE` or `OPTIONAL`
- [ ] **Normative source:** which file becomes semantic authority for this area
- [ ] **Compiler obligation level:** required now vs deferred
- [ ] **Test tier:** parser-only, pipeline-required, or release-blocking
- [ ] **Migration policy:** if optional now, whether to remove or isolate syntax

### Gate Criteria (Weighted)

1. End-to-end implementability in current `jazz-hs` architecture.
2. Consistency with already locked spec-cleanup decisions (syntax/class/map/purity).
3. User-facing documentation truthfulness (avoid feature over-claim).
4. Testability with reproducible Nix commands.
5. Cost/risk of keeping parse-only constructs alive.

## Spec/Testing Priority Rules By Outcome

### If Outcome = `CORE`

- [ ] Spec priority P0: define normative semantics for `data`, constructors, `case`, pattern forms, and match failure/exhaustiveness policy.
- [ ] Testing priority P0: add parser + type inference + codegen + runtime smoke coverage for ADT/pattern workflows.
- [ ] Reliability requirement: no catch-all `not implemented` path for committed ADT/pattern forms.
- [ ] Documentation rule: README may claim ADT/pattern as implemented only after pipeline support exists.

### If Outcome = `OPTIONAL`

- [ ] Spec priority P0: mark ADT/pattern as non-core (experimental, parse-only, or deprecated).
- [ ] Testing priority P0: enforce explicit boundary behavior (either parser rejection or clearly labeled experimental parser-only tests).
- [ ] Reliability requirement: remove ambiguous claims from user-facing feature lists.
- [ ] Cleanup rule: either remove unsupported syntax or isolate behind explicit non-core status with no implied runtime contract.

## Phased Clarification Plan

### Phase 0: Freeze Evidence and Draft Decision Matrix

- [ ] Create an evidence-backed matrix for each construct:
  - `data` declarations
  - constructor invocation
  - constructor patterns
  - list/tuple patterns
  - `case` expressions
  - pattern parameters in lambdas
- [ ] For each row, capture: parser support, inference support, codegen support, runtime support, tests, docs claims.
- [ ] Mark each row as `Core Candidate`, `Optional Candidate`, or `Undecided`.

**Commit checkpoint**

Message:
`docs(spec-clarification-11): add adt-pattern evidence matrix and decision gate`

Exact `git add` targets:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/semantics/11-evidence-matrix.md
```

### Phase 1: Positioning Decision Record (Core vs Optional)

- [ ] Run a decision pass using the gate criteria and matrix from Phase 0.
- [ ] Produce one authoritative decision record with:
  - selected positioning (`CORE` or `OPTIONAL`)
  - rationale tied to concrete evidence
  - explicit non-goals and deferrals
  - required next-plan branches.
- [ ] Cross-link this decision to existing spec-cleanup item #6 planning so ownership is not split.

**Commit checkpoint**

Message:
`docs(spec-clarification-11): lock adt-pattern positioning decision`

Exact `git add` targets:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/semantics/11-decision-record.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/compiler/06-parse-only-features-resolution.md
```

### Phase 2A: Core-Commitment Execution Track (Run Only If `CORE`)

- [ ] Write normative semantics sections:
  - ADT declaration and constructor typing
  - Pattern match resolution order
  - `case` branch selection semantics
  - wildcard/default behavior
  - exhaustiveness policy (required warning/error behavior).
- [ ] Add failing tests first for committed behavior:
  - parser acceptance
  - type inference for constructor/case/pattern params
  - JS generation for committed forms
  - runtime smoke examples.
- [ ] Implement missing analyzer/codegen behavior in minimal slices, each with tests.
- [ ] Update README/state docs to keep ADT/pattern claims consistent with shipped behavior.

**Commit checkpoint A1 (spec)**

Message:
`docs(spec): define core adt-pattern semantics and obligations`

Exact `git add` targets:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/adt-pattern-semantics.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/semantics/11a-core-adt-pattern-semantics.md
```

**Commit checkpoint A2 (tests first)**

Message:
`test(adt-pattern): add failing end-to-end expectations for core commitment`

Exact `git add` targets:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Analyzer/TypeInferenceSpec.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Spec.hs
```

**Commit checkpoint A3 (implementation)**

Message:
`feat(adt-pattern): implement committed inference/codegen semantics`

Exact `git add` targets:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/ScopeAnalyzer.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Javascript.hs
```

### Phase 2B: Optional-Scaffolding Execution Track (Run Only If `OPTIONAL`)

- [ ] Decide strict handling mode per construct:
  - remove parser support, or
  - keep parser support but mark as non-core/unsupported (explicitly).
- [ ] Align tests with selected handling mode:
  - parser rejection tests for removed forms, or
  - parser-only experimental tests with explicit non-core markers.
- [ ] Remove README language that implies production support.
- [ ] Keep language-state/spec docs explicit about non-core status and future decision point.

**Commit checkpoint B1 (policy/spec)**

Message:
`docs(spec): classify adt-pattern as optional scaffolding and define boundaries`

Exact `git add` targets:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/feature-status.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/semantics/11b-optional-adt-pattern-scaffolding.md
```

**Commit checkpoint B2 (parser/tests alignment)**

Message:
`refactor(parser): align adt-pattern surface with optional-scaffolding policy`

Exact `git add` targets:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Spec.hs
```

### Phase 3: Testing-Priority Enforcement (Both Tracks)

- [ ] Introduce an explicit ADT/pattern test matrix with priority labels:
  - `P0` release-blocking
  - `P1` required but non-blocking
  - `P2` exploratory/experimental.
- [ ] Ensure matrix matches chosen positioning:
  - `CORE`: ADT/pattern pipeline tests become `P0`.
  - `OPTIONAL`: boundary/absence tests become `P0`.
- [ ] Wire targeted test commands into reproducible Nix command list.

**Commit checkpoint**

Message:
`test(strategy): enforce adt-pattern priority matrix from positioning decision`

Exact `git add` targets:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/testing/adt-pattern-priority-matrix.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Spec.hs
```

### Phase 4: Documentation Closure and Traceability

- [ ] Update top-level language claims to match chosen positioning.
- [ ] Record closure note in language-state/spec-cleanup docs with commit references.
- [ ] Ensure future contributors can tell, from docs alone, whether ADT/pattern semantics are mandatory or optional.

**Commit checkpoint**

Message:
`docs: close adt-pattern positioning ambiguity with traceable status`

Exact `git add` targets:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/README.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/feature-status.md
```

## Nix-Based Verification Commands

Run from repo root unless noted otherwise.

### Environment Readiness

```bash
nix --version
nix develop ./jazz-hs -c stack --version
nix develop ./jazz-hs -c ghc --version
```

### Baseline (Current Behavior Snapshot)

```bash
nix develop ./jazz-hs -c stack test --test-arguments "--match \"Constructor Tests\""
nix develop ./jazz-hs -c stack test --test-arguments "--match \"Simple Lambda Tests\""
nix develop ./jazz-hs -c stack test --test-arguments "--match \"Simple Expression Type Inference Tests\""
nix develop ./jazz-hs -c stack test
```

### Core Track Verification (If `CORE`)

```bash
nix develop ./jazz-hs -c stack test --test-arguments "--match \"case\""
nix develop ./jazz-hs -c stack test --test-arguments "--match \"constructor pattern\""
nix develop ./jazz-hs -c bash -lc 'cd jazz-hs && stack run -- ExamplePrograms/ComplexProgram.jz > /tmp/jazz-core-track.js'
nix develop ./jazz-hs -c node /tmp/jazz-core-track.js
```

### Optional Track Verification (If `OPTIONAL`)

```bash
nix develop ./jazz-hs -c stack test --test-arguments "--match \"Statement Tests\""
nix develop ./jazz-hs -c stack test --test-arguments "--match \"Constructor Tests\""
nix develop ./jazz-hs -c stack test
```

### Final Sanity

```bash
git -C /Users/admin/.codex/worktrees/8c77/jazz-main diff --name-only
```

## Planned Branch-Out Documents (Create Only As Needed)

- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/semantics/11-evidence-matrix.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/semantics/11-decision-record.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/semantics/11a-core-adt-pattern-semantics.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/semantics/11b-optional-adt-pattern-scaffolding.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/testing/11c-adt-pattern-test-priority.md`

## Short Checkbox Summary

- [x] Ambiguity evidence captured from specs + old code
- [x] Core-vs-optional decision gate defined
- [x] Phased plan includes commit messages and exact `git add` targets
- [x] Nix-based verification command matrix included
- [x] Branch-out plan paths listed for follow-up research/execution
