---
id: JN-PATTERN-OR-SEMANTICS-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-06-26
plan_section: "Implementation batch: Or-pattern semantics"
target_paths:
  - docs/spec/pattern-matching-semantics.md
  - docs/spec/adt-pattern-semantics.md
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/src/JazzNext/Compiler/AST.hs
  - jazz-next/src/JazzNext/Compiler/Analyzer.hs
  - jazz-next/src/JazzNext/Compiler/ModuleResolver.hs
  - jazz-next/src/JazzNext/Compiler/Driver.hs
  - jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Implement case-arm or-patterns as one pattern form with equal binder sets and compatible binder types across alternatives, left-to-right runtime alternative matching, guard/body visibility for common binders only, and no pattern synonyms, guard expansion, solver behavior, or exhaustiveness analysis."
---

# Jazz-Next Or-Pattern Semantics Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** implement one future pattern form, case-arm or-patterns, in
`jazz-next` without reopening guards, pattern synonyms, generic solver behavior,
or legacy compiler paths.

**Architecture:** add a single surface/core pattern node for alternatives and
keep it inside the existing `EPatternCase` pipeline. The parser recognizes
top-level `|` separators inside one case-arm pattern before the optional guard
or body arrow, type inference checks every alternative against the same
scrutinee and exposes only compatible common binders, and runtime tries
alternatives left-to-right before guard/body selection.

**Tech Stack:** Haskell modules under `jazz-next/src/JazzNext/Compiler`, focused
`runghc` suites under `jazz-next/test/JazzNext/Compiler`, active pattern specs
under `docs/spec/`, and repo-root queue/docs validation.

---

## Implementation batch: Or-pattern semantics

### Contract

Surface syntax:

- Accept top-level case-arm or-patterns with the existing `|` token:

  ```jz
  case value {
    | Just item | Also item if item > 0 -> item
    | Nothing -> 0
  }
  ```

- The first `|` still starts the case arm. Any later top-level `|` before that
  arm's optional `if <guard-expr>` or `->` separates alternatives inside the
  same pattern.
- Each alternative is one currently accepted non-or case pattern: literal,
  wildcard, variable, constructor, exact-length list, cons-like list,
  fixed-arity tuple, or as-pattern.
- Case-arm guards remain optional `if <guard-expr>` expressions after the whole
  or-pattern. Or-patterns do not add multiple guards or guard-local binders.
- This child does not add pattern synonyms, grouping patterns, nested
  or-patterns inside constructor/list/tuple/as-pattern subpatterns, lambda
  parameter or-patterns, exhaustiveness analysis, or match-compilation
  optimizations.

Binder and type rules:

- Typecheck each alternative against the same scrutinee type.
- All alternatives in one or-pattern must bind exactly the same set of names.
  `Just item | Nothing` is rejected because the second alternative does not bind
  `item`.
- Duplicate binders inside one alternative keep the existing duplicate case
  pattern binder diagnostic.
- Reusing the same binder name in separate alternatives is not a duplicate; it
  is required for a bound name to be visible to the guard and arm body.
- For each common binder, infer the binder type for every alternative and unify
  those types. If a binder has incompatible alternative types, reject the
  or-pattern with deterministic `E2011` text naming the binder.
- The arm guard and body see only the compatible common binders.
- Arm result agreement remains body-owned through the existing `E2012` path.
- Do not add inferred class constraints, broad defaulting, solver-backed
  constrained signatures, explicit type application, runtime dictionaries, or
  primitive mixed-width behavior.

Runtime rules:

- Runtime tries alternatives left-to-right inside the current arm.
- The first matching alternative produces pattern bindings for the whole arm.
- If no alternative matches, the whole pattern fails and runtime continues with
  the next case arm.
- If an alternative matches and the arm has a guard, evaluate the guard with the
  alternative's bindings. `True` selects the arm, `False` falls through to the
  next arm, and a failed pattern skips the guard.
- If no arm is selected after pattern and guard checks, keep the existing
  `E3022` no-match diagnostic.

### Target paths

- `docs/spec/pattern-matching-semantics.md`: record the accepted case-arm
  or-pattern contract and remaining non-goals.
- `docs/spec/adt-pattern-semantics.md`: include or-patterns in the active
  pattern subset only after implementation lands.
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`: add
  `SPOr [SurfacePattern]`.
- `jazz-next/src/JazzNext/Compiler/Parser.hs`: parse top-level case-arm
  alternatives without changing case-arm delimiters, guard parsing, list cons
  syntax, or pipe operators in arm bodies.
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`: lower `SPOr` to `POr`.
- `jazz-next/src/JazzNext/Compiler/AST.hs`: add `POr [Pattern]`.
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`: include or-pattern binders in
  case-arm binder collection with common-binder semantics.
- `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`: collect constructor
  references from all alternatives and treat only common binders as arm-local
  names for guard/body reference resolution.
- `jazz-next/src/JazzNext/Compiler/Driver.hs`: rewrite imported constructor
  references and collect pattern constructor references through all
  alternatives.
- `jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs`: traverse alternative
  patterns for recursive binding analysis.
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`: typecheck alternative
  patterns, enforce equal binder sets, unify common binder types, preserve
  duplicate-binder handling inside each alternative, and expose final common
  binders to guards/bodies.
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`: match alternatives
  left-to-right and return the first successful alternative's bindings.
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`: add parser
  and lowering coverage for accepted or-patterns, guard boundaries, and a
  malformed alternative.
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`: add
  compile/type coverage for common binders, binder-set mismatch, incompatible
  binder types, guard visibility, and duplicate binders inside one alternative.
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs`: add
  runtime coverage for left-to-right alternative selection, fallback when all
  alternatives fail, guard false fallthrough, and existing `E3022` no-match.
- `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`: cover an imported
  constructor referenced through an or-pattern alternative so module traversal
  and constructor rewriting stay aligned.

### Focused implementation steps

- [ ] **Step 1: Write parser/lowering tests**

  Add parser expectations for the accepted shape:

  ```haskell
  parseSurfaceProgram
    "x = case value { | Just item | Also item if item > 0 -> item | Nothing -> 0 }."
  ```

  Expected surface pattern for the first arm:

  ```haskell
  SPOr
    [ SPConstructor "Just" [SPVariable "item"],
      SPConstructor "Also" [SPVariable "item"]
    ]
  ```

  Expected lowered pattern for the first arm:

  ```haskell
  POr
    [ PConstructor "Just" [PVariable "item"],
      PConstructor "Also" [PVariable "item"]
    ]
  ```

  Also add a parser regression that a pipe expression in the arm body remains a
  body expression boundary case and a malformed alternative such as
  `case value { | Just item | -> item }.` reports a parser diagnostic.

- [ ] **Step 2: Add AST and lowering nodes**

  Add constructors:

  ```haskell
  | SPOr [SurfacePattern]
  ```

  ```haskell
  | POr [Pattern]
  ```

  Lower with:

  ```haskell
  SPOr patterns -> POr (map lowerSurfacePattern patterns)
  ```

- [ ] **Step 3: Parse top-level case-arm alternatives**

  Refactor case-arm pattern parsing so `parseCaseArm` calls an or-aware helper
  before guard parsing:

  ```haskell
  (casePattern, afterPattern) <- parseCaseArmPattern tokensAfterPipe
  ```

  The helper parses one existing non-or pattern and then consumes one or more
  top-level `TOperator "|"` separators only while another valid pattern follows
  before the arm guard or `->`. It must leave these existing forms unchanged:

  ```jz
  case values { | [head | tail] -> head | [] -> 0 }.
  case value { | Just item if item > 0 -> item | _ -> 0 }.
  case value { | Just item -> item |> f | _ -> 0 }.
  ```

- [ ] **Step 4: Thread pattern traversals**

  Update every existing pattern traversal with a `POr` or `SPOr` case:

  ```haskell
  POr alternatives ->
    Set.union bound (commonPatternBinderNames alternatives)

  commonPatternBinderNames :: [Pattern] -> Set Text
  commonPatternBinderNames alternatives =
    case alternatives of
      [] -> Set.empty
      firstAlternative : rest ->
        foldl'
          Set.intersection
          (patternBinderNames firstAlternative)
          (map patternBinderNames rest)
  ```

  Use a common-binder helper instead of unioning all binders for guard/body
  scope. Constructor/reference traversals should visit every alternative.

- [ ] **Step 5: Add type tests before implementation**

  Add a positive common-binder source:

  ```jz
  data Maybe = Nothing | Just value | Also value.
  value = Also 41.
  case value { | Just item | Also item -> item + 1 | Nothing -> 0 }.
  ```

  Add a guard-visibility source:

  ```jz
  data Maybe = Nothing | Just value | Also value.
  value = Just 4.
  case value { | Just item | Also item if item > 0 -> item | Nothing -> 0 }.
  ```

  Add rejections for these sources:

  ```jz
  data Maybe = Nothing | Just value.
  value = Nothing.
  case value { | Just item | Nothing -> item | _ -> 0 }.
  ```

  ```jz
  pair = (True, 0).
  case pair { | (item, 0) | (True, item) -> item | _ -> 0 }.
  ```

  ```jz
  pair = (1, 2).
  case pair { | (item, item) | (left, right) -> item | _ -> 0 }.
  ```

- [ ] **Step 6: Implement type inference**

  Add `inferOrPatternType` from `inferPatternType`:

  ```haskell
  POr alternatives ->
    inferOrPatternType env scrutineeType alternatives state
  ```

  The helper should:

  - reject an empty alternative list defensively with `E2011`;
  - infer each alternative against the original scrutinee type;
  - run the existing duplicate-binder check on each alternative;
  - reject unequal binder sets with `E2011`;
  - unify each common binder type across alternatives;
  - return one `PatternTyping` containing only common binders and the updated
    inference state.

- [ ] **Step 7: Add runtime tests before implementation**

  Add passing runtime sources:

  ```jz
  data Maybe = Nothing | Just value | Also value.
  value = Also 41.
  case value { | Just item | Also item -> item + 1 | Nothing -> 0 }.
  ```

  ```jz
  case 3 { | 1 | 2 -> 10 | _ -> 20 }.
  ```

  ```jz
  data Maybe = Nothing | Just value | Also value.
  value = Also 2.
  case value { | Just item | Also item if item > 3 -> 1 | _ -> 0 }.
  ```

  Add a no-match source that preserves `E3022`:

  ```jz
  case 3 { | 1 | 2 -> 10 }.
  ```

- [ ] **Step 8: Implement runtime matching**

  Add:

  ```haskell
  POr alternatives -> matchFirstAlternative alternatives
  ```

  `matchFirstAlternative` should try alternatives in source order and return
  the first `RuntimeEnv` produced by `matchPattern`. It should return `Nothing`
  only when every alternative fails.

- [ ] **Step 9: Update specs and run verification**

  Update `docs/spec/pattern-matching-semantics.md` and
  `docs/spec/adt-pattern-semantics.md` after parser/type/runtime tests pass.
  Run:

  ```bash
  bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
  bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
  bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
  bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

### Self-review

- Spec coverage: the plan covers the blocker-required syntax, binder
  compatibility, type behavior, runtime behavior, diagnostics, target paths,
  and focused verification for one pattern form only.
- Scope check: guards, pattern synonyms, nested/grouped or-patterns, lambda
  parameter or-patterns, solver behavior, exhaustiveness, and legacy compiler
  paths stay out of scope.
- Queue/frontmatter parity: the frontmatter values match the Ready Now row
  proposed for central integration.
