---
id: JN-MEGAPARSEC-SIMPLIFICATION-001
status: in_progress
priority: P2
size: M
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/Parser/
  - test/Jazz/Compiler/Parser/
  - jazz.cabal
verification:
  - cabal test all --jobs=4 --test-show-details=failures
deliverable: "Replace manual lexer and parser mechanics with library combinators"
last_verified: 2026-09-09
---

# Megaparsec simplification

The user requested an independent lexer/parser simplification pass and explicitly
authorized parser-combinators where useful. Execute inline in the current
worktree from `b4b9edbf`. Preserve accepted syntax, structured failure reasons,
consumption/backtracking, source ranges, lexemes, and hosted frontend parity.

## Audit and design

- Lexer: use `space`, `skipLineComment`, `lexeme`, `decimal`, `manyTill`, and
  central `match` capture instead of repeated token records and manual loops.
- Token adapter: use `satisfy`, `eof`, and `region` to retain Jazz's existing
  structured errors at the primitive boundary.
- Expression/pattern/signature lists: use `sepBy1`, `many`, `between`, and
  parser-combinators' nonempty `sepBy1` where their consumption matches.
- Preserve custom escape validation (`charLiteral` accepts different escapes),
  precedence climbing (contextual pipes and mixed associativity), indexed token
  stream, declaration-boundary lookahead, and duplicate-name checking.
- Use existing parser and hosted parity fixtures; add focused boundary cases
  only where needed to protect the changed combinator behavior.

## Implementation

- [ ] Simplify lexer, token adapter, and list grammars; add direct dependency.
- [ ] Verify exact failures, ranges, literals, syntax, and hosted parity.
- [ ] Run full default suite and formatting/lint/package/repository checks.
- [ ] Record evidence, commit, and close dispatcher.
