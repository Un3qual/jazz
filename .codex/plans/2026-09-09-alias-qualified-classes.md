---
id: JN-MODULE-ALIAS-QUALIFIED-CLASSES-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/ModuleResolver.hs
  - src/Jazz/Compiler/ModuleAnalysis.hs
  - jazz/compiler/ParserExpression.jz
verification:
  - cabal test all --jobs=4 --test-show-details=failures
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Alias-qualified class methods, constraints and impl heads preserve identity and visibility across both frontends."
last_verified: 2026-09-09
---

# Alias-qualified Classes Implementation Plan

Use the subagent-driven-development workflow for independent frontend work;
keep integration and final verification in this existing isolated worktree.

**Goal:** Implement the maintainer-approved RFC 0017 as a complete module batch.

**Architecture:** Extend structured source names, then resolve class and method
references to original module identities before inference. Import the same
public class facts and evidence for qualified access as for unqualified access.
Preserve existing runtime dispatch and explicit module publication boundaries.

**Tech stack:** Haskell, hosted Jazz, pinned Nix development and quality shells.

**Spec:** `rfcs/accepted/0017-alias-qualified-classes.md`, approved in this task.

## Global constraints

- Exactly two components for a qualified class and three for a qualified method.
- Preserve class identity, private visibility, impl policy and existing syntax.
- Keep module aliases, classes and methods structurally distinguishable.
- Match Haskell and hosted parser/canonical-core behavior.
- No re-exports, imported operators, new class features or backend work.
- Commit milestones and close queue/public documentation together at completion.

## Implementation

### Task 1: Source syntax and canonical names

Owners: `src/Jazz/Compiler/Name.hs`, `src/Jazz/Compiler/Parser/AST.hs`,
`src/Jazz/Compiler/Parser/Expression.hs`,
`src/Jazz/Compiler/Parser/CapabilityDeclaration.hs`,
`src/Jazz/Compiler/Parser/Lower.hs`, parser and canonical comparison tests.

- [ ] Add failing parser cases for `Facts::Eq::equals 1 1`,
      `same :: @{Facts::Eq(a)}: a -> a -> Bool.`, and a qualified impl head.
      Verify malformed and four-component names and qualified class declarations
      fail. Use the expression/declaration/parser-foundation suites.
- [ ] Represent a three-part method explicitly in the surface and source-name
      types; lower to the existing variable expression with that source name.
      Retain separate two-component names. Extend impl header parsing to retain
      qualification and preserve source ranges and purity of the final member.
- [ ] Update exhaustive consumers and canonical adapters. Verify parser suites
      and commit the coherent syntax change once integration compiles.

### Task 2: Qualified module identity, visibility and evidence

Owners: `src/Jazz/Compiler/ModuleResolver.hs`,
`src/Jazz/Compiler/ModuleResolver/Imports.hs`,
`src/Jazz/Compiler/ModuleResolver/Names.hs`,
`src/Jazz/Compiler/ModuleExports.hs`, `src/Jazz/Compiler/ModuleAnalysis.hs`,
`test/Jazz/Compiler/Modules/Loader/CapabilitiesTests.hs`.

- [ ] First establish the existing loader baseline, then add a failing graph:
      `import Lib::Facts as Facts. Facts::Eq::equals 1 1.` with an exported
      `Eq` class and concrete integer implementation; assert output `True`.
- [ ] Collect qualified class references separately from type/value references;
      validate against explicit aliases and public capability inventories.
      Resolve the structured method to its module-owned class/method identity.
      Qualified constraint and impl names use the same class namespace lookup.
- [ ] Include public classes in aliased interfaces. Deduplicate repeated access
      to the same evidence identity while retaining distinct conflicting impls.
      Verify direct/stored/partial/explicit methods, constrained functions,
      qualified impls for local ADTs, and hidden evidence transport.
- [ ] Verify different same-text classes, two aliases and mixed imports,
      private/missing classes and methods, namespace mismatch, unqualified
      isolation, duplicate impl errors, and no re-exports. Commit verified work.

### Task 3: Hosted frontend parity

Owners: `jazz/compiler/ParserTypes.jz`, `jazz/compiler/ParserExpression.jz`,
`jazz/compiler/ParserDeclaration.jz`, `jazz/compiler/CoreTypes.jz`,
`jazz/compiler/CoreLower.jz`, hosted/canonical adapter and comparison suites.

- [ ] Add shared source corpus cases for all three forms and malformed names;
      run canonical-parser/core comparison to establish missing behavior.
- [ ] Mirror the agreed structured method and qualified impl representation,
      adjacent-component grammar and canonical lowering. Retain old forms.
- [ ] Run canonical parser/core comparisons plus hosted declaration/expression
      suites; commit parity once the complete source pipeline passes.

### Task 4: Public contract, integration and closeout

- [ ] Add an executable aliased-class module example and document qualified
      methods, constraints and impl heads in module/capability/grammar references.
      Correct the existing overly broad claim that imported methods cannot run.
- [ ] Run all supported suites, examples, formatter/linter, quality and repository
      checks through pinned shells; resolve failures against their root cause.
- [ ] Review the complete diff for RFC coverage and maintainability, resolve
      verified findings, and rerun affected checks.
- [ ] Mark this plan complete, record actual checks and commits, remove the ready
      row, reconcile blocker contracts and shipped status, and commit closeout.

## Execution record

- Maintainer approved RFC 0017. Existing isolated detached worktree retained.
- Local default Cabal lacks a Hackage index; use the pinned Nix shell, invoked
  through `/nix/var/nix/profiles/default/bin/nix` because Nix is absent from PATH.
