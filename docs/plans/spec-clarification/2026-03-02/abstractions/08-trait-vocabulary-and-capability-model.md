# Trait Vocabulary and Capability Model Clarification Plan (Item 08)

> Focus: resolve abstraction vocabulary and capability-model ambiguity.
>
> Explicitly out of scope: the abstraction keyword choice (`class` vs `trait`) is already approved separately as `class` and is not reopened here.

## Progress

- [x] Collected contradictions from old code and current specs.
- [x] Defined scope boundary excluding the approved `class` keyword decision.
- [ ] Choose canonical vocabulary model (`Eq`/`Ord`/`Num` family vs `Collection`/`Orderable` family).
- [ ] Specify canonical capability model semantics (declaration, implementation, solver behavior, defaulting, extensibility).
- [ ] Align docs/spec text to one vocabulary and one capability narrative.
- [ ] Add repeatable validation checks and close this clarification item.

## Scope Guardrails

In scope:
- Vocabulary model for abstractions/capabilities (naming taxonomy and surface terminology).
- Capability model clarity (what is syntax-only vs executable semantics, and what is open vs closed).
- Constraint model clarity (`@{...}:` constraints, instance/declaration semantics, defaulting policy).

Out of scope:
- Canonical abstraction keyword decision (`class` already approved).
- Re-litigating map/filter argument order (already approved separately).
- Broad parse-only cleanup beyond capability-related surfaces (tracked in item #6 plan).

## Verification Evidence (Concrete Contradictions/Gaps)

1. Vocabulary contradiction between aspirational docs and implementation authority.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md:5` advertises approachable typeclasses like `Collection`, `Orderable`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs:106`-`112` hardcodes traits as `Num`, `Integral`, `Fractional`, `Eq`, `Ord`, `Showable`, `Default`.
- Gap: no canonical naming model is defined for which family is normative.

2. The state doc explicitly records unresolved vocabulary/capability ambiguity.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:316` states abstraction names/model are not stable.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:392` lists unresolved choice: Haskell-like (`Eq`/`Ord`/`Num`) vs domain-like (`Collection`/`Orderable`).
- Gap: unresolved item remains open despite other decisions being locked.

3. Prelude vocabulary is mixed and not in one coherent naming tier.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz:24` uses `trait Ord(a)`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz:43` uses `trait Eq(a)`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz:50` uses `trait Collection(a)`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz:64` uses `class Num(a)`.
- Gap: vocabulary + abstraction surface are both mixed in one file, preventing clear capability taxonomy.

4. Capability syntax and prelude instance forms are incompatible.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs:124`-`129` expects constraints as `@{...}:`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs:165`-`171` expects `impl @{...}: Class(Type) { ... }`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz:79` uses `impl Num(Int)` and `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz:91` uses `impl Eq Int`.
- Gap: capability implementation form is not normalized, so vocabulary decisions cannot be validated against executable grammar.

5. Parser supports class/impl AST nodes, but analyzer does not implement their semantics.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs:158`-`171` parses typeclass declarations/impls.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs:131`-`178` handles only `ELet`, `ELiteral`, `EVar`, `EApply`, `ELambda`, `ETypeSignature`, `EBlock`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs:179`-`181` falls through to runtime error for other nodes.
- Gap: capability declaration model is parse-level but not semantics-level.

6. Capability extensibility is unclear (parse-open but solver-closed).
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs:127` parses arbitrary constructor constraints in `@{...}`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs:230`-`234` rejects traits not in `traitsTable` (`TraitNotInScopeError`).
- Gap: syntax suggests open trait naming; solver enforces closed hardcoded registry.

7. Defaulting behavior exists but is undocumented as part of capability semantics.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs:236`-`240` selects default type per trait from `traitsTable`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs:362`-`381` applies defaulting during constraint solving.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Analyzer/TypeInferenceSpec.hs:118`-`129` expects `+` specialization to `Integer -> Integer -> Integer`.
- Gap: no documented policy for when defaulting is desired vs surprising.

8. Collection capability naming exists in docs/prelude, but compiler capability registry has no corresponding trait.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz:49`-`53` defines `Collection(a)` trait.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs:106`-`112` has no `Collection` or `Orderable` entries.
- Gap: collection capability naming is not represented in executable trait/capability tables.

9. Approved keyword decision is documented separately and must remain excluded here.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/README.md:24` locks canonical keyword to `class`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md:24` confirms `class` as canonical.
- Scope consequence: this clarification item addresses naming/capability semantics only.

## Clarification Targets (What Must Become Unambiguous)

1. Canonical vocabulary tiering.
- Which trait names are normative in docs and examples.
- Whether alternate names are aliases, pedagogical labels, or deprecated terms.

2. Capability model semantics.
- Whether capabilities are closed-world builtins, open-world user-extensible, or staged (closed now, open later).
- Semantics of declaration vs implementation vs usage constraints.

3. Constraint and solver contract.
- Relationship between parsed constraints, registered capabilities, and solver behavior.
- Explicit defaulting policy and failure conditions.

4. Conformance model.
- Which source is authoritative for capability vocabulary/semantics.
- How docs/tests prevent drift.

## Decision Gates (Executor-Flexible)

### Gate A: Vocabulary Model

- [ ] Option A1: Haskell-core canonical (`Eq`, `Ord`, `Num`), domain names as aliases/docs wrappers.
- [ ] Option A2: Domain-core canonical (`Collection`, `Orderable`, `Numeric`), Haskell names as compatibility aliases.
- [ ] Option A3: Two-tier model: compiler-internal canonical names + user-facing canonical aliases with explicit mapping table.

Decision criteria:
- Minimal conflict with existing executable compiler behavior.
- Clarity for new users reading docs/examples.
- Migration cost for existing docs/prelude/tests.
- Future extensibility without renaming churn.

### Gate B: Capability Model Openness

- [ ] Option B1: Closed-world now (hardcoded registry is normative); parser/docs must reflect that.
- [ ] Option B2: Staged openness (closed execution today + explicit roadmap and syntax for open user-defined capabilities).
- [ ] Option B3: Fully open now (implement user-defined capability declarations/instances in analyzer/solver).

Decision criteria:
- Current implementation feasibility.
- Spec honesty (implemented vs planned semantics).
- Error quality and debuggability.
- Consistency with parse-only feature strategy.

### Gate C: Defaulting Policy

- [ ] C1: Keep implicit defaulting, but document deterministic rules and scope.
- [ ] C2: Restrict defaulting to explicit opt-in contexts.
- [ ] C3: Remove defaulting and require explicit annotation in ambiguous cases.

Decision criteria:
- Predictability for users.
- Backward compatibility with current tests/examples.
- Long-term type-system ergonomics.

## Phased Execution Plan With Commit Checkpoints

### Phase 0: Clarification Packet Setup (this item)

- [x] Record contradiction evidence and scoped decision gates.
- [ ] Confirm owner/reviewers and acceptance rubric for this item.

Commit checkpoint:
- Message: `plan(spec): frame trait vocabulary and capability-model clarification`
- Exact add targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/abstractions/08-trait-vocabulary-and-capability-model.md
```

### Phase 1: Vocabulary Decision Artifact

- [ ] Create a dedicated decision matrix artifact with side-by-side naming mappings and examples.
- [ ] Select one canonical vocabulary model from Gate A and document non-canonical term handling.
- [ ] Define compatibility wording (`alias`, `deprecated alias`, or `historical note`) for every non-canonical term.

Primary file targets:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/abstractions/08a-vocabulary-decision-matrix.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/abstractions/trait-vocabulary.md`

Commit checkpoint:
- Message: `docs(spec): decide canonical abstraction vocabulary model`
- Exact add targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/abstractions/08a-vocabulary-decision-matrix.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/abstractions/trait-vocabulary.md
```

### Phase 2: Capability Model Specification

- [ ] Select openness model from Gate B and codify expected runtime/typechecker behavior.
- [ ] Define declaration/implementation/constraint semantics in one normative section.
- [ ] Select and document defaulting policy from Gate C with examples and failure cases.
- [ ] State authoritative source order (`compiler behavior`, `spec docs`, `examples`) to prevent future drift.

Primary file targets:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/abstractions/08b-capability-model-semantics.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/abstractions/capability-model.md`

Commit checkpoint:
- Message: `docs(spec): define capability-model semantics and defaulting policy`
- Exact add targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/abstractions/08b-capability-model-semantics.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/abstractions/capability-model.md
```

### Phase 3: Conformance Alignment Track Selection

Choose one track after Phases 1-2.

#### Track 3A: Docs-First Convergence (if semantics remain mostly closed-world)

- [ ] Align high-level docs to canonical vocabulary/capability semantics.
- [ ] Remove contradictory wording from language-state unresolved lists once resolved.
- [ ] Keep implementation status explicit where behavior remains intentionally limited.

File targets:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/abstractions/trait-vocabulary.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/abstractions/capability-model.md`

Commit checkpoint:
- Message: `docs(lang): align abstraction vocabulary and capability semantics`
- Exact add targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/README.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/abstractions/trait-vocabulary.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/abstractions/capability-model.md
```

#### Track 3B: Compiler-Conformance Alignment (if open/staged capability semantics are chosen)

- [ ] Align parser/inference/tests with chosen capability semantics.
- [ ] Ensure trait registry/solver behavior matches declared openness and defaulting policy.
- [ ] Add tests that enforce chosen vocabulary and capability behavior.

File targets (select exact subset actually changed):
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Analyzer/TypeInferenceSpec.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz`

Commit checkpoint:
- Message: `feat(typeclass): align compiler capability behavior with clarified model`
- Exact add targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Analyzer/TypeInferenceSpec.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz
```

### Phase 4: Closure and Cross-Plan Consistency

- [ ] Update spec-cleanup status docs to reference resolved vocabulary/capability item.
- [ ] Ensure item #8 is no longer listed as unresolved in language-state docs.
- [ ] Attach final validation evidence logs/command output references.

File targets:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/README.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/abstractions/08-trait-vocabulary-and-capability-model.md`

Commit checkpoint:
- Message: `chore(spec): close abstraction vocabulary and capability-model clarification`
- Exact add targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/README.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/abstractions/08-trait-vocabulary-and-capability-model.md
```

## Nix-Based Reproducible Validation Commands

Run from repo root (`/Users/admin/.codex/worktrees/8c77/jazz-main`).

1. Baseline vocabulary inventory.
```bash
nix shell nixpkgs#ripgrep nixpkgs#coreutils --command zsh -lc 'cd /Users/admin/.codex/worktrees/8c77/jazz-main && rg -n "\\b(Collection|Orderable|Eq|Ord|Num|Integral|Fractional|Showable|Default)\\b" README.md docs jazz-hs/src jazz-hs/static jazz-hs/test --glob "!jazz-hs/local-deps/**"'
```

2. Constraint-surface inventory (`@{...}:` and class/impl grammar references).
```bash
nix shell nixpkgs#ripgrep nixpkgs#coreutils --command zsh -lc 'cd /Users/admin/.codex/worktrees/8c77/jazz-main && rg -n "@\\{|typeclassDeclP|typeclassImplP|traitsTable|defaultTraitType" jazz-hs/src/Parser/Lang.hs jazz-hs/src/Types.hs jazz-hs/src/Analyzer/TypeInference.hs jazz-hs/static/Prelude.jz'
```

3. Parser capability syntax regression slice.
```bash
nix shell nixpkgs#stack nixpkgs#ghc --command zsh -lc 'cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs && stack test --test-arguments="--match typeclass"'
```

4. Full compiler test baseline after any capability/vocabulary change.
```bash
nix shell nixpkgs#stack nixpkgs#ghc --command zsh -lc 'cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs && stack test'
```

5. Documentation drift check after convergence.
```bash
nix shell nixpkgs#ripgrep nixpkgs#coreutils --command zsh -lc 'cd /Users/admin/.codex/worktrees/8c77/jazz-main && rg -n "Collection|Orderable|Eq|Ord|Num|capability model|trait vocabulary" README.md docs/spec docs/jazz-language-state.md'
```

## Suggested Follow-Up Plan Files (if decomposition is needed)

- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/abstractions/08a-vocabulary-decision-matrix.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/abstractions/08b-capability-model-semantics.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/abstractions/08c-conformance-and-migration.md`

## Risks and Mitigations

- [ ] Risk: picking names without matching solver behavior creates another docs/code mismatch.
  Mitigation: Gate B and Gate C decisions must land before README/spec rewrites.
- [ ] Risk: parser remains more permissive than solver registry.
  Mitigation: explicitly choose and test open vs closed capability model.
- [ ] Risk: old terminology survives in examples and reintroduces ambiguity.
  Mitigation: include vocabulary drift grep checks in Nix validation.

## Short Checkbox Summary

- [x] Evidence-backed contradictions and gaps documented with exact paths.
- [x] Detailed phased plan added with commit messages and exact `git add` targets.
- [x] Nix-based reproducible validation commands included.
- [ ] Await Gate A/B/C decisions to execute the implementation track.
