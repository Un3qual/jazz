# Test validity and passing-suite repair

Latest maintainer request: get all tests passing and determine whether existing
tests remain necessary and valid. Continue on `codex/simplify-haskell-compiler`
from `4ad818a1`; do not use Ponytail. This request authorizes the narrow hosted
parser parity work needed by retained tests, beyond the earlier audit deferral.

- [x] Reproduce failures and check their expectations against accepted RFCs,
      current public contracts and the actual artifacts under test.
- [x] Repair the hosted signature collector/classifier using one complete
      payload probe with the Haskell parser's qualification and delimiter rules.
- [x] Preserve three-part qualified method expressions in hosted parsing and
      canonical lowering, including current malformed-name diagnostics.
- [x] Replace stale test labels and assertions that only inspect unchanged
      inputs; retain valid behavioral/parity checks.
- [x] Run all 66 configured suites, including the four opt-in full-scale suites,
      serially with the pinned Nix toolchain; resolve any additional valid failures.
- [x] Review changes independently, finish relevant quality checks, update the
      dispatcher/verification record and commit.

The ten known failures compare hosted output with the live Haskell parser;
they are valid parity checks, not obsolete backend snapshots. RFCs 0016/0018
retain hosted/canonical structural tests. RFC 0017 records the missing parity as
deferred work; it does not make the assertions wrong. Some test names still
described old parser behavior and were corrected without weakening assertions.

Use GHC 9.14.1 and Cabal 3.16.1.0 through the pinned Nix environment. Run only one
Cabal process at a time (`--jobs=1`) because concurrent invocations previously
collided in named semaphore handling on this host.

## Changes and test validity decisions

- Retained all ten failing parity cases. A fresh baseline reproduced all ten;
  they pass after the hosted parser fixes.
- Replaced overlapping compact-signature scanners with one complete payload
  parser shared by signature probing, ordinary signatures, class methods and
  operator signatures. Qualified constraint heads now receive the same strict
  validation as Haskell while unsupported legacy payloads remain representable.
- Added the missing three-component qualified-method surface/core variants and
  lowering, with shared member/method adjacency diagnostics. Preserved strict
  type-name adjacency as well.
- Removed one exact duplicate canonical-if lowering test. Updated three stale
  hosted parser labels and two backend-oriented labels without changing their
  assertions.
- Replaced three assertions over the unchanged inference input with checks of
  the actual analyzed conditional and qualified/unqualified source ranges.
- Removed `inferExpressionDefault` and its production Weeder exception after
  the corrected assertions eliminated its last callers. The full unused-code
  check identified this now-obsolete test convenience wrapper.
- Added focused name/constraint boundary coverage and one composed hosted
  parsing/lowering fixture; promoted the existing qualified-constraint case out
  of a grouped test so it cannot hide later cases when it fails.
- Independent reviews found no further invalid retained tests or defects in the
  final payload validator, collector, compact probe rollback or dot consumption.
- The hosted production implementation is 44 lines smaller across six files;
  removing the unused Haskell helper saves another four lines in `src/Jazz`.

## Verification

- Focused suites passed: hosted declaration/signature/module parsing, hosted
  signature/declaration/operator core lowering, source ranges, if parsing and
  core normalization. The declaration suite includes all ten original failures.
- All 66 suites passed with no skipped performance cases, using the pinned Nix
  toolchain and `cabal test all -ffull-parser-scale --jobs=1 --test-show-details=direct`.
- All four full-scale suites retained their original workloads and every limit.
  Their limits are post-run assertions, not execution timeouts. Each completed
  with the expected 513 statements, successful termination and no host operations.

| Full-scale workload | Evaluator transitions | Existing ceiling |
| ------------------- | --------------------: | ---------------: |
| Operator            |            49,733,434 |       52,000,000 |
| Expression          |            21,995,264 |       22,000,000 |
| Declarations        |             9,695,267 |       80,000,000 |
| Control flow        |            42,438,319 |       45,000,000 |

- Repository checks passed, including the main repository phase, CI policy,
  documentation/workflow checker tests, website-boundary checker tests and
  Ormolu for the six edited Haskell test files. Corrected formatting drift in
  the two earlier simplification records.
- The clean Haskell quality gate passed: HLint, production and full Weeder,
  fresh builds of all components including full-scale suites and benchmarks,
  and generated invariants. The only finding in the initial quality run was
  the obsolete `inferExpressionDefault` helper; the clean rerun passed after
  removing it and its allowlist entry.
- After that deletion, `source-ranges-spec` and `core-normalization-spec` passed
  again. All components compiled in the clean quality run; the prior full-scale
  results remain applicable because no reachable implementation changed.
- The final CLI build, `cabal check`, all five checked examples and executable
  public documentation checks passed. Benchmark and profiling runs were not
  part of this repair; no performance speedup is claimed.

Implementation and test corrections are committed in `1fff17b5`; public/internal
status reconciliation is committed in `4ba02fd5`. The final closeout commit also
removes the now-unused inference helper and records the completed verification.
