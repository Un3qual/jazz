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
- [ ] Run all 66 configured suites, including the four opt-in full-scale suites,
      serially with the pinned Nix toolchain; resolve any additional valid failures.
- [ ] Review changes independently, finish relevant quality checks, update the
      dispatcher/verification record and commit.

The ten known failures compare hosted output with the live Haskell parser;
they are valid parity checks, not obsolete backend snapshots. RFCs 0016/0018
retain hosted/canonical structural tests. RFC 0017 records the missing parity as
deferred work; it does not make the assertions wrong. Some test names still
describe old parser behavior and will be corrected without weakening assertions.

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
- Added focused name/constraint boundary coverage and one composed hosted
  parsing/lowering fixture; promoted the existing qualified-constraint case out
  of a grouped test so it cannot hide later cases when it fails.
- Independent reviews found no further invalid retained tests or defects in the
  final payload validator, collector, compact probe rollback or dot consumption.

## Verification

- Focused suites passed: hosted declaration/signature/module parsing, hosted
  signature/declaration/operator core lowering, source ranges, if parsing and
  core normalization. The declaration suite includes all ten original failures.
- Full suite and final quality/repository verification are in progress.
