---
id: JN-MEGAPARSEC-SIMPLIFICATION-001
status: complete
priority: P2
size: M
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/Parser/Lexer.hs
  - test/Jazz/Compiler/Parser/TokenParserSpec.hs
  - jazz.cabal
verification:
  - cabal test all --jobs=4 --test-show-details=failures
deliverable: "Preserve syntax and diagnostics while removing manual parser mechanics"
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

- [x] Simplify lexer, token adapter, and list grammars; add direct dependency.
- [x] Verify exact failures, ranges, literals, syntax, and hosted parity.
- [x] Run full default suite and formatting/lint/package/repository checks.
- [x] Record evidence, commit, and close dispatcher.

## Results and evidence

Production parser code is 214 lines shorter across seven files. The lexer uses
one `match` capture for original lexemes and ranges, `space`/`skipLineComment`/
`lexeme` for trivia, `decimal` for arbitrary-precision integers, `manyTill` for
quoted bodies, and `reachOffsetNoLine` for fallback error positions. The token
adapter uses `satisfy`/`eof` with `region` to retain structured errors, and `match`
for consumed token extents. Lists, tuples, constructor arguments, or-pattern
tails, case arms, and lambda parameters use standard repetition combinators.
Signature wrappers use `between`. Removed redundant `lookAhead getInput` calls
and the obsolete quoted-body HLint exception.

Added `parser-combinators >= 1.3 && < 1.4` for
`Control.Monad.Combinators.NonEmpty.sepBy1` in lambda parameter parsing.
Megaparsec already reexports the ordinary list combinators. Kept
`InvalidIntegerLiteral` in the public lexical-failure vocabulary because hosted
canonical adapters expose it, while removing the unreachable manual decimal
conversion failure path.

Preserved precedence climbing, custom Unicode/escape validation, indexed stream
instances, and declaration loops that enforce duplicate-name policy or scope and
recovery boundaries. This changes no public syntax or hosted schemas.

- Full default matrix: all 62 suites passed; command included
  `--enable-tests --keep-going --jobs=4 --test-show-details=failures
--ghc-options=-fwrite-ide-info`. Log: `/tmp/jazz-megaparsec-all.log`.
- Direct comparison with `b4b9edbf`: exact old/new agreement on 54,241 lexer
  inputs and 11,478 parser inputs, including 93 repository Jazz sources,
  exhaustive short inputs, and truncated declarations/expressions. Compared
  complete shown results including errors and ranges. Baseline modules and
  comparison harness were isolated in `/tmp/jazz-parser-oracle`; no duplicate
  implementation added to the repository. Log: `/tmp/jazz-megaparsec-oracle.log`.
- Added tests for alternative commitment and error ranges, large zero-padded
  integers, Unicode whitespace/comments, raw carriage returns, and rejected
  Haskell-only escapes. Existing fixtures cover trailing separators, operator
  behavior, nested patterns, and hosted parity.
- HLint across `src app test benchmark program-support`: no hints. Ormolu
  on all changed Haskell files and `git diff --check` passed.
- Initial focused build caught a local name-shadowing error; corrected before
  the successful focused/full runs. Queue checker caught initial metadata
  mismatches; corrected to exact file targets, matching deliverable, and `ready`
  status, then passed.
- Fresh quality gate passed with `JAZZ_CABAL_JOBS=4 nix develop .#quality
--command bash scripts/ci/haskell-quality.sh`: HLint, isolated production HIE
  build/Weeder, full test/benchmark/opt-in scale build/Weeder, and generated
  invariants. Log: `/tmp/jazz-megaparsec-quality.log`.
- Tested dependency versions: Megaparsec 9.7.0 and parser-combinators 1.3.1.
- `cabal check` and `cabal sdist` passed; source archive is
  `/tmp/jazz-megaparsec-sdist/jazz-0.1.0.0.tar.gz`. Log:
  `/tmp/jazz-megaparsec-package.log`.
- Implementation commit: `adeea4ca`; plan commit: `63486c4f`. No push or PR was
  requested. The dispatcher closes this independent batch with no new candidate.
- Final documentation/authority/clarification/queue checks and their regression
  suites passed after pinned Markdown formatting. Log:
  `/tmp/jazz-megaparsec-docs.log`.
