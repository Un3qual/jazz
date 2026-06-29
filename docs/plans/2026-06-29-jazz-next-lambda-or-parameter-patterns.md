---
id: JN-PATTERN-LAMBDA-OR-PARAMETERS-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on:
  - JN-PATTERN-OR-SEMANTICS-001
last_verified: 2026-06-29
plan_section: "Implementation batch: Lambda-parameter or-patterns"
target_paths:
  - docs/spec/pattern-matching-semantics.md
  - docs/spec/adt-pattern-semantics.md
  - docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Accept or-pattern alternatives in pattern-shaped lambda parameters by reusing the active `POr` binder/type/runtime contract and existing lambda-pattern lowering to internal single-arm `EPatternCase`; no lambda guards, nested/grouped or-patterns, pattern synonyms, exhaustiveness, or solver behavior."
---

# Jazz-Next Lambda-Parameter Or-Patterns

**Goal:** accept top-level or-pattern alternatives in pattern-shaped lambda
parameters, reusing the active case-arm `POr` contract and existing lambda
pattern lowering.

**Architecture:** lambda pattern parameters already lower to ordinary unary
lambdas whose bodies perform an internal single-arm `EPatternCase`. Case-arm
or-patterns already define equal binder sets, compatible binder types,
left-to-right matching, guard/body visibility, and `E3022` no-match behavior.
This child makes the lambda-parameter parser consume the same or-aware pattern
shape while keeping lambda guards, nested/grouped or-patterns, pattern synonyms,
and exhaustiveness analysis out of scope.

**Tech Stack:** Haskell parser and pattern semantics under `jazz-next/`, focused
parser/type/runtime/module `runghc` suites, and active pattern specs under
`docs/spec/`.

---

## Source Evidence

- `JN-PATTERN-OR-SEMANTICS-001` landed top-level case-arm or-patterns with
  binder/type/runtime coverage.
- `docs/spec/pattern-matching-semantics.md` says lambda parameter patterns lower
  through internal single-arm `EPatternCase`.
- The parser still treats lambda parameters as the active non-or pattern subset,
  so `\(Just item | Also item) -> item` is a parser-local missing slice rather
  than a new type/runtime design.

## Implementation batch: Lambda-parameter or-patterns

Accepted shape:

```jazz
choose = \(Just item | Also item) -> item.
```

Rules:

- Accept top-level or-pattern alternatives in pattern-shaped lambda parameters.
- Reuse the existing `POr` binder rules: every alternative must bind the same
  names, and common binder types must unify.
- Reuse the existing runtime rules: alternatives match left to right, and no
  matching alternative in the internal single-arm case reports the existing
  no-match diagnostic.
- Keep ordinary non-or lambda parameter patterns unchanged.
- Keep nested/grouped or-patterns and lambda guards rejected.

Implementation outline:

1. Add parser/lowering coverage in `LambdaParserSpec.hs` for a lambda parameter
   that lowers to `POr`.
2. Add type and runtime coverage for successful matching, binder-set mismatch,
   incompatible binder types, and no-match behavior.
3. Add module coverage for imported constructors referenced in lambda
   or-pattern alternatives.
4. Change lambda pattern parsing in `Parser.hs` to use the or-aware helper while
   still stopping at the existing lambda parameter delimiters.
5. Update pattern specs and the active ADT/pattern rebase plan.

Out of scope:

- lambda guards,
- nested/grouped or-patterns,
- pattern synonyms,
- exhaustiveness analysis,
- generic solver/defaulting behavior,
- match-compilation optimization,
- any `jazz-hs/` or `jazz2/` work.

## Verification

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```
