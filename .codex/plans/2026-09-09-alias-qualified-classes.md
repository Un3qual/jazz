---
id: JN-MODULE-ALIAS-QUALIFIED-CLASSES-001
status: complete
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/ModuleResolver.hs
  - src/Jazz/Compiler/ModuleAnalysis.hs
  - src/Jazz/Compiler/Parser/Expression.hs
verification:
  - cabal test loader-spec parser-foundation-spec source-ranges-spec module-resolution-spec module-exports-spec module-pipeline-contract-spec --jobs=4 --test-show-details=failures
  - JAZZ_CABAL_JOBS=4 bash scripts/ci/haskell-quality.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Alias-qualified class methods, constraints and impl heads preserve identity and visibility in the Haskell compiler."
last_verified: 2026-09-09
---

# Alias-qualified Classes Implementation Plan

Execute the remaining implementation, review and verification inline in this
existing isolated worktree, as requested by the maintainer during implementation.

**Goal:** Implement the maintainer-approved RFC 0017 as a complete module batch.

**Architecture:** Extend structured source names, then resolve class and method
references to original module identities before inference. Import the same
public class facts and evidence for qualified access as for unqualified access.
Preserve existing runtime dispatch and explicit module publication boundaries.

**Tech stack:** Haskell, pinned Nix development and quality shells.

**Spec:** `rfcs/accepted/0017-alias-qualified-classes.md`, approved in this task.

## Global constraints

- Exactly two components for a qualified class and three for a qualified method.
- Preserve class identity, private visibility, impl policy and existing syntax.
- Keep module aliases, classes and methods structurally distinguishable.
- Hosted frontend and bootstrap feature work are explicitly deferred.
- No re-exports, imported operators, new class features or backend work.
- Commit milestones and close queue/public documentation together at completion.

## Implementation

### Task 1: Source syntax and canonical names

Owners: `src/Jazz/Compiler/Name.hs`, `src/Jazz/Compiler/Parser/AST.hs`,
`src/Jazz/Compiler/Parser/Expression.hs`,
`src/Jazz/Compiler/Parser/CapabilityDeclaration.hs`,
`src/Jazz/Compiler/Parser/Lower.hs`, parser and canonical comparison tests.

- [x] Add failing parser cases for `Facts::Eq::equals 1 1`,
      `same :: @{Facts::Eq(a)}: a -> a -> Bool.`, and a qualified impl head.
      Verify malformed and four-component names and qualified class declarations
      fail. Use the expression/declaration/parser-foundation suites.
- [x] Represent a three-part method explicitly in the surface and source-name
      types; lower to the existing variable expression with that source name.
      Retain separate two-component names. Extend impl header parsing to retain
      qualification and preserve source ranges and purity of the final member.
- [x] Update exhaustive consumers and canonical adapters. Verify parser suites
      and commit the coherent syntax change once integration compiles.

### Task 2: Qualified module identity, visibility and evidence

Owners: `src/Jazz/Compiler/ModuleResolver.hs`,
`src/Jazz/Compiler/ModuleResolver/Imports.hs`,
`src/Jazz/Compiler/ModuleResolver/Names.hs`,
`src/Jazz/Compiler/ModuleExports.hs`, `src/Jazz/Compiler/ModuleAnalysis.hs`,
`test/Jazz/Compiler/Modules/Loader/CapabilitiesTests.hs`.

- [x] First establish the existing loader baseline, then add a failing graph:
      `import Lib::Facts as Facts. Facts::Eq::equals 1 1.` with an exported
      `Eq` class and concrete integer implementation; assert output `True`.
- [x] Collect qualified class references separately from type/value references;
      validate against explicit aliases and public capability inventories.
      Resolve the structured method to its module-owned class/method identity.
      Qualified constraint and impl names use the same class namespace lookup.
- [x] Include public classes in aliased interfaces. Deduplicate repeated access
      to the same evidence identity while retaining distinct conflicting impls.
      Verify direct/stored/partial/explicit methods, constrained functions,
      qualified impls for local ADTs, and hidden evidence transport.
- [x] Verify different same-text classes, two aliases and mixed imports,
      private/missing classes and methods, namespace mismatch, unqualified
      isolation, duplicate impl errors, and no re-exports. Commit verified work.

### Task 3: Hosted frontend parity — deferred by maintainer

The maintainer removed all bootstrap-related work from the active batch.
Discard the draft hosted parser/lowering changes and new parity cases. Retain
only exhaustive Haskell test-adapter cases needed for existing suites to build.
Run existing suites as regression checks without extending the hosted profile.

### Task 4: Public contract, integration and closeout

- [x] Add an executable aliased-class module example and document qualified
      methods, constraints and impl heads in module/capability/grammar references.
      Correct the existing overly broad claim that imported methods cannot run.
- [x] Run all supported suites, examples, formatter/linter, quality and repository
      checks through pinned shells; resolve failures against their root cause.
- [x] Review the complete diff for RFC coverage and maintainability, resolve
      verified findings, and rerun affected checks.
- [x] Mark this plan complete, record actual checks and commits, remove the ready
      row, reconcile blocker contracts and shipped status, and commit closeout.

## Execution record

- Maintainer approved RFC 0017. Existing isolated detached worktree retained.
- Local default Cabal lacks a Hackage index; use the pinned Nix shell, invoked
  through `/nix/var/nix/profiles/default/bin/nix` because Nix is absent from PATH.
- After the maintainer requested inline execution, both agents were stopped.
  Remaining implementation, review, fixes and verification ran inline.
- The maintainer deferred bootstrap work. Draft changes under `jazz/compiler/`
  and new hosted fixtures were discarded. Only exhaustive Haskell canonical
  adapter cases remain so existing suites can build.
- Parser and loader regressions went from failure to passing. Inline review
  also reproduced and fixed missing-method source locations for both direct
  calls and stored method values. Import validation locates alias/class tokens
  using the existing lexical pass; resolution preserves defining identities.
- Feature, executable example and public contract committed as `b36baa72`.

## Verification

- All 47 non-bootstrap suites passed. The broad run passed 45; the source
  inventory expectation was then updated for the new examples, and
  `repository-audit-spec` plus the unscheduled `warning-config-spec` passed in
  targeted follow-ups. Earlier compiler errors in exhaustive test adapters were
  fixed and verified before this run.
- `cabal build all --jobs=4` passed. The quality gate additionally built every
  test and benchmark, including opt-in parser scale suites, in a fresh directory.
- `JAZZ_CABAL_JOBS=4 bash scripts/ci/haskell-quality.sh` in the pinned quality
  shell passed: HLint reported no hints, production and full Weeder scans passed,
  and generated invariant tests passed.
- Ormolu checks passed for changed Haskell files. `scripts/check-examples.sh`
  passed all examples and public documentation execution, including
  `qualified-class` with output `(True, False, True)`.
- `scripts/check-docs.sh`, `scripts/check-execution-queue.sh`, and
  `git diff --check` passed. No hosted source implementation changes remain.

The non-bootstrap suite selection is reproducible in the pinned development
shell with:

```bash
cabal test $(awk '
  /^[a-z]/ { suite = "" }
  /^test-suite / { suite = $2 }
  /main-is:/ && suite != "" && $0 !~ /\/Bootstrap\// { print suite }
' jazz.cabal) --jobs=4 --test-show-details=failures
```

## Deferred hosted parity

The full `cabal test all --jobs=4 --test-show-details=failures` regression run
found ten failures in `jazz-parser-types-declarations-modules-spec`. Its
comparisons cover qualified constraints, parenthesized qualified signature
payloads, newly accepted three-component methods, and overlong qualification
diagnostics. The Haskell grammar now accepts or diagnoses those forms differently
from the unchanged hosted parser. These failures remain explicitly deferred;
the full bootstrap-inclusive suite is not green. No hosted tests were removed
or weakened to hide this difference.

## Review follow-up — 2026-09-10

The maintainer requested fixes for both reproduced review findings. Work stayed
inline, with bootstrap still deferred.

- Qualified constraint validation now descends through grouping parentheses
  while retaining delimiter depth, so spacing and excess qualification cannot
  bypass validation. Regressions cover nested grouping, a later grouped
  constraint, malformed qualification, and valid grouped qualification.
- Explicit class-method instantiation now attaches the method expression's
  source span to new unlocated errors. Regressions check missing methods in
  both stored explicit instantiations and explicitly instantiated calls.
- Focused loader, parser-foundation, source-range and structured-diagnostic
  suites passed after reproducing the failures. HLint and Ormolu checks passed.
- Final verification: all 47 non-bootstrap suites passed, along with plan
  formatting, execution-queue validation and `git diff --check`.

## Code-quality review follow-up — 2026-09-10

The maintainer requested the remaining source-location fix. The loader
regression reproduced a class diagnostic pointing to an earlier, same-spelled
type argument in the same signature (column 21 instead of 39).

The constraint parser's existing validation scan now also returns qualified
head tokens and the unconsumed suffix. The resolver reuses those heads when
building its source-location index, excluding type arguments. This fixes the
ambiguity without changing the shared surface-signature representation or
adding a second constraint grammar. Regressions cover both ordinary and nested
parenthesized constraint heads. Work remained inline; bootstrap stayed deferred.

All 47 non-bootstrap suites passed. A subsequent edge-case regression exposed
an unfinished legacy constraint block consuming later statements in the span
scan; the shared scan now stops at the statement terminator. After that final
correction, loader, parser-foundation, source-range and structured-diagnostic
suites passed again. Final HLint and Ormolu checks passed, as did plan formatting,
execution-queue validation and `git diff --check`.

## PR 155 review follow-through — 2026-09-10

The initial snapshot contained all 12 review threads, six review summaries and six PR comments;
every connection was complete without further pagination. The inventory includes
CodeRabbit's outside-diff finding and CodeAnt's top-level nitpick. Duplicate
reports were evaluated together, and bootstrap remains maintainer-deferred.

| Finding                                                                                                      | Disposition                                                                                                                                                                                            |
| ------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| CodeAnt: hard-coded status date (`3981380847`)                                                               | Replaced the exact-date assertion with the existing YYYY-MM-DD format requirement; no freshness policy added.                                                                                          |
| CodeAnt: qualified result type rejected after an arrow (`3981393241`)                                        | Reproduced and fixed the signature-boundary predicate to permit qualified names where a type can begin.                                                                                                |
| Cubic: nested declaration swallowed (`3981509075`); CodeRabbit outside-diff: premature dot (`5169932770`)    | Reproduced and fixed boundary detection at every depth and rejection of terminators inside unclosed delimiters. Retained the existing depth counter instead of introducing a delimiter-stack redesign. |
| CodeRabbit/Cubic: non-identifier constraint alias (`3981469011`, `3981509048`)                               | Added explicit alias-token rejection and regressions, including grouped heads. Merely restricting the successful pattern would still fall through to unsupported-signature handling.                   |
| CodeRabbit/Cubic: broad rejection assertions (`3981469038`, `3981509106`)                                    | Replaced the helper accepting any parser failure with assertions for each intended diagnostic.                                                                                                         |
| CodeAnt nitpick/Cubic: RFC 0017 outside its table (`5622167267`, `3981509091`)                               | Removed the table-breaking blank line.                                                                                                                                                                 |
| Cubic: method span applied to argument errors (`3981509100`)                                                 | Reproduced E2007 at the method rather than the list argument. Annotated argument-inference errors before method-resolution errors; existing specific spans remain intact.                              |
| CodeAnt/Cubic: hosted core schema (`3981393765`, `3981509087`); CodeAnt: hosted parser schema (`3981393774`) | Valid parity gaps already documented above; deferred under the maintainer's bootstrap exclusion. No hosted code or canonical adapters changed.                                                         |

The focused loader, parser-foundation, source-range and structured-diagnostic
suites passed after reproducing the behavioral failures. The earlier unfinished
signature location regression now asserts its intentional parser rejection.

Final verification passed all 47 non-bootstrap suites, HLint, Ormolu, the
documentation gate, plan formatting, execution-queue validation and
`git diff --check`. The documentation gate passed on a serialized rerun after
its first concurrent run hit one-second fixture timeouts and inspected the plan
before formatting completed.
