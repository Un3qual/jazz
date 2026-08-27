# Jazz Haskell typeclass audit M1: composition implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan.

**Goal:** Replace current bespoke empty, merge, and enumeration machinery with lawful, actively used standard Haskell instances while preserving ordering and collision semantics.

**Architecture:** Instances live with their data declarations so their laws and ownership stay local. Public behavior tests cover private accumulator types; a focused contract suite covers exported accumulators. No representation is exported solely for testing.

**Tech Stack:** Haskell 2010 with explicit module extensions, GHC 9.14.1, Cabal, repository `Jazz.TestHarness`, Nix development shell.

## Global constraints

- Follow `.codex/plans/2026-08-15-jazz-haskell-typeclass-audit-design.md`.
- Add only instances with an immediate call-site or safety benefit.
- Preserve left bias, first-occurrence ordering, and outer-before-inner hint order.
- Write the behavioral assertion before changing each implementation group.
- Run suites serially; repository test executables can contend for resources.
- Format every touched Haskell file and commit each green group.

---

## Task 1: Exported accumulator contracts

**Files:**

- Create: `test/Jazz/Compiler/HaskellTypeclassContractsSpec.hs`
- Modify: `jazz.cabal`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Types.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower/Requirements.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Types.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Capabilities.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Operator.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Scope.hs`
- Modify: `src/Jazz/Compiler/ModuleExports.hs`

- [x] Add a `haskell-typeclass-contracts-spec` Cabal suite using `test-common`.
- [x] Define `assertMonoidLaws :: (Eq a, Show a, Monoid a) => Text -> a -> a -> a -> IO ()` in the new test module.
- [x] Test `RuntimeRequirements` identities, associativity, Boolean disjunction, and service-set union.
- [x] Test `ScopeCapabilityFacts` identities, associativity, left-biased class/method collisions, and left-to-right implementation-method concatenation. Use `ClassMethodType "Left" (SignatureType TypeInt)` and `ClassMethodType "Right" (SignatureType TypeBool)` so collision precedence is observable without new exports.
- [x] Test `ModuleExportInventory` identities, associativity, and duplicate elimination through `exportInventoryEntries`.
- [x] Run `nix --extra-experimental-features 'nix-command flakes' develop --command cabal test haskell-typeclass-contracts-spec -fdevelopment` and confirm compilation fails because the three `Monoid` instances do not exist.
- [x] Add `Semigroup` and `Monoid` to `RuntimeRequirements` in `Lower/Types.hs`, preserving `False` plus `Set.empty` as `mempty`.
- [x] Rewrite `Requirements.hs` with `mempty`, `(<>)`, and `foldMap`; delete `emptyRuntimeRequirements` and `mergeRuntimeRequirements` and remove obsolete imports.
- [x] Add `Semigroup` and `Monoid` to `ScopeCapabilityFacts` in `TypeInference/Types.hs`. Keep `emptyScopeCapabilityFacts = mempty` as a domain-named construction alias.
- [x] Delete `mergeCapabilityFacts` from `Capabilities.hs` and its export list; replace all call sites in `Capabilities.hs`, `Operator.hs`, and `Scope.hs` with `(<>)` while preserving operand order.
- [x] Add `Semigroup` and `Monoid` to `ModuleExportInventory`; rewrite selector accumulation in `ModuleExports.hs` with `foldMap` without unwrapping and rewrapping the set.
- [x] Format all touched Haskell files.
- [x] Rerun the focused suite plus `binding-signature-coherence-spec` and `jazz-typed-core-contract-spec` with `-fdevelopment`.
- [x] Commit the test and implementation files as `refactor: standardize compiler accumulators`.

## Task 2: Private accumulator behavior and simplification

**Files:**

- Modify: `src/Jazz/Compiler/RecursiveBindings.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Pattern.hs`
- Modify: `src/Jazz/Compiler/ModuleCompiler.hs`
- Modify: `src/Jazz/Compiler/Runtime/Types.hs`
- Test: `test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs`
- Test: `test/Jazz/Compiler/Semantics/PatternSemanticsSpec.hs`
- Test: `module-resolution-spec`, `module-exports-spec`, `module-pipeline-contract-spec`, and `runtime-semantics-spec`

- [x] Run the recursive-binding, pattern, module-import, and runtime characterization suites before edits. These public tests are the red/characterization boundary because the private constructors must remain private.
- [x] Add `Semigroup` and `Monoid` to private `OrderedNames`. Preserve first occurrence by inserting the right sequence into the left accumulator. Replace `orderedNamesEmpty`, `orderedNamesUnion`, and `orderedNamesUnions` with `mempty`, `(<>)`, `mconcat`, or `foldMap`, then delete the helpers.
- [x] Add `Semigroup` and `Monoid` to private `PatternBindings` using left-biased `Map.union` and to private `PatternTyping` using binding composition plus `(||)`. Replace and delete the four parallel empty and merge helpers.
- [x] Add `Semigroup` and `Monoid` to private `ImportedInterface`, delegating capability facts to their instance and preserving left-biased maps plus set union. Replace dependency accumulation with `foldMap`; delete `mergeModuleInterfaces`.
- [x] Add only `Semigroup` to `RuntimeExplicitResultHints`, backed by `Seq.><`. Rewrite nested hint attachment with `(<>)`; do not add `Monoid`.
- [x] Rerun every characterization suite and format the three files accepted by the pinned formatter. `Runtime/Types.hs` retains its established formatting because the formatter cannot parse GHC 9.14's valid `data VExplicitResultHints` export syntax; the development build and runtime suite gate that file.
- [x] Confirm `rg -n 'orderedNames(Empty|Union|Unions)|emptyPatternBindings|mergePatternBindings|emptyPatternTyping|mergePatternTyping|mergeModuleInterfaces' src test` finds no deleted helper.
- [x] Commit the four source files as `refactor: use lawful private accumulators`.

## Task 3: Exhaustive numeric enumeration

**Files:**

- Modify: `src/Jazz/Compiler/Parser/AST.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs`

- [x] Add an assertion that `[minBound .. maxBound]` equals the exact current order: `SurfaceNumericInt8`, `SurfaceNumericInt16`, `SurfaceNumericInt32`, `SurfaceNumericInt64`, `SurfaceNumericUInt8`, `SurfaceNumericUInt16`, `SurfaceNumericUInt32`, `SurfaceNumericUInt64`, `SurfaceNumericFloat16`, `SurfaceNumericFloat32`, `SurfaceNumericFloat64`.
- [x] Run `canonical-parser-comparison-spec` and confirm compilation fails because `SurfaceNumericType` lacks `Bounded` and `Enum`.
- [x] Derive `Enum` and `Bounded` for `SurfaceNumericType` and replace the hand-maintained `allNumericTypes` constructor list with `[minBound .. maxBound]`.
- [x] Format both files, rerun `canonical-parser-comparison-spec`, and commit as `refactor: derive numeric surface enumeration`.

## Task 4: M1 closeout

- [x] Confirm superseded helpers are gone and each accepted instance has a current consumer with focused `rg` searches.
- [x] Run the pinned formatter across every changed Haskell file it can parse. `Runtime/Types.hs` is covered by the documented GHC 9.14 parser exception.
- [x] Run `nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all -fdevelopment --test-show-details=direct --test-option=--sequential`.
- [x] Run `nix --extra-experimental-features 'nix-command flakes' develop --command cabal check` and `git diff --check a7e1cf61`.
- [x] Record exact verification results below and commit the evidence update as `docs: record typeclass composition verification`.

## Verification evidence

- The deleted-helper search returned no matches.
- Every changed Haskell file accepted by the pinned Ormolu parser passed `--mode check`; `Runtime/Types.hs` cannot be parsed by that Ormolu version because it uses GHC 9.14's valid `data VExplicitResultHints` export syntax.
- `cabal test all -fdevelopment --test-show-details=direct --test-option=--sequential` completed successfully. It produced 126 current suite logs; a failure scan found no failed suite or `FAIL:` entry.
- `cabal check` reported: `No errors or warnings could be found in the package.`
- `git diff --check a7e1cf61` completed without output.
