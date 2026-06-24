---
id: JN-PATTERN-GUARD-SEMANTICS-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on:
  - JN-PATTERN-GUARD-CONTRACT-001
last_verified: 2026-06-24
plan_section: "Implementation batch: Pattern guard semantics"
target_paths:
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
deliverable: "Implement optional `if` guard expressions on case arms with pattern binders visible to Bool guard checking and runtime evaluation, `False` falling through to later arms, existing `E3022` no-match behavior, and no or-patterns, pattern synonyms, guard-introduced binders, or solver behavior."
supersedes:
  - docs/plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md
---

# Jazz-Next Pattern Guard Implementation

> Implementation child for the accepted guard-only contract
> `JN-PATTERN-GUARD-CONTRACT-001`. All code changes belong in `jazz-next/`.

**Goal:** implement optional `if` guard expressions on `case` arms without
changing the existing active pattern forms or adding broader matcher features.

**Architecture:** extend case-arm data to carry an optional guard expression.
Parsing/lowering preserves the guard, traversal owners visit it where arm bodies
are already visited, type inference checks it as `Bool` in the pattern-extended
arm environment, and runtime evaluates it only after the pattern has matched.

**Tech Stack:** Haskell modules under `jazz-next/src/JazzNext/Compiler`,
focused `runghc` suites under `jazz-next/test/JazzNext/Compiler`, and repo-root
queue/docs validation.

---

## Implementation batch: Pattern guard semantics

Executor-safe scope:

- Parse guarded case arms with shape `| <pattern> if <guard-expr> -> <body>`.
- Preserve unguarded arms unchanged.
- Carry optional guard expressions through surface AST, core AST, and lowering.
- Visit guard expressions in analyzer/module/driver/recursive-binding
  traversals wherever case arm bodies are already visited.
- Typecheck guard expressions as `Bool` after pattern binders are available.
- Keep guard expressions out of arm result agreement.
- Runtime-match the pattern first; skip guard/body on pattern failure; evaluate
  guard on pattern success; select body on `True`; fall through on `False`.
- Preserve existing `E3022` no-match behavior when all patterns fail or guards
  evaluate to `False`.

Expected coverage:

- Parser accepts a guarded constructor arm and preserves an unguarded fallback.
- Parser rejects malformed guard syntax.
- Type inference lets guard expressions reference pattern binders.
- Type inference rejects non-`Bool` guards with a guard-specific diagnostic.
- Runtime falls through from a matched pattern when its guard evaluates to
  `False`.
- Runtime does not evaluate a guard when the pattern fails.
- Runtime emits `E3022` when every matching pattern has a `False` guard.
- Existing unguarded literal, wildcard, variable, constructor, list, tuple,
  as-pattern, and lambda-parameter pattern tests remain green.

Out of scope:

- or-patterns,
- pattern synonyms,
- multiple guard clauses per arm,
- guard-introduced binders,
- exhaustiveness analysis,
- match-compilation optimizations,
- inferred class constraints, broad defaulting, solver-backed constrained
  signatures, runtime dictionaries, explicit type application, or primitive
  mixed-width behavior,
- any `jazz-hs/` or `jazz2/` work.

Verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```
