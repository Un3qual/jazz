# Jazz-Next Constructor Exports and Broad Standard Library Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. This plan is
> intentionally outcome-oriented: it locks behavior, interfaces, tests,
> commands, and commit boundaries without pasting the final implementation.

**Goal:** Add grouped data-constructor exports and a broad Jazz-authored
functional standard library with persistent collections, text utilities,
compiler dogfooding, and performance evidence.

**Architecture:** Parse grouped type selectors as structured source metadata,
validate constructor ownership in module resolution, and expand them into the
existing flat `ModuleExportInventory` before compiler/runtime publication.
Keep public stdlib datatypes in focused Jazz modules, hide persistent
collection representations behind explicit export lists, and add only the
narrow runtime primitives required for representation-level operations.

**Tech Stack:** GHC 9.14.1, Haskell 2010 plus project-local extensions, Cabal,
Jazz source under `jazz-next/jazz`, the existing parser/resolver/interpreter,
TextMate JSON, the program corpus, and `tasty-bench`.

## Global Constraints

- Follow the approved design in
  `docs/superpowers/specs/2026-07-15-jazz-next-constructor-exports-broad-stdlib-design.md`.
- Modify compiler implementation only under `jazz-next/`; treat `jazz-hs/` and
  `jazz2/` as read-only references.
- Preserve `type T`, existing per-namespace selectors, explicit export-none,
  export-all compatibility, local private visibility, and the flat downstream
  export inventory.
- Use `type T(..)` for all constructors and `type T(C1, C2)` for a selected
  constructor subset. Do not introduce a `data` export selector.
- Keep stdlib public functions module-prefixed and review every added public
  capability, datatype, and operation name against the approved vocabulary.
- Prefer total APIs returning `Maybe` for normal absence or empty-collection
  outcomes.
- Implement algorithms in Jazz unless the current runtime representation makes
  a narrow kernel bridge necessary for correctness or asymptotic behavior.
- Do not add a public `TextBuilder`, hashing contract, mutable collection,
  superclass, default class method, or category-theory-named abstraction.
- Preserve deterministic dictionary insertion order, ascending map/set order,
  stable list sorting, persistent structural sharing, and documented queue
  amortization limits.
- Use external `.jz` fixtures for substantial programs. When a focused parser
  or diagnostic test embeds Jazz, use `MultilineStrings`; reserve explicit
  newline concatenation for tests that directly exercise whitespace or spans.
- Add behavior tests before production changes and commit each milestone.
- Treat benchmark results as evidence, not a fixed percentage pass/fail gate.
- Do not replace or promote the independent parser-design target in
  `docs/execution/queue.md`.

---

## File and Responsibility Map

### Compiler export pipeline

- `jazz-next/src/JazzNext/Compiler/ModuleExports.hs`: structured export
  selectors, group rendering, flat inventory expansion helpers.
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`: source-level grouped selector
  retention.
- `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`: grouped export
  grammar, item spans, and syntax diagnostics.
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`: source-qualified selector
  lowering.
- `jazz-next/src/JazzNext/Compiler/ModuleGraph.hs`: located declared-export
  metadata retained until resolution.
- `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`: owned data-constructor
  validation and expansion to the public inventory.

### Prelude and runtime boundary

- `jazz-next/jazz/stdlib/Prelude.jz`: `Ordering`, `Ord::compare`,
  `Showable::show`, `Default::defaultValue`, and primitive implementations.
- `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`: canonical inventory for
  new private kernel symbols.
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`: builtin type schemes.
- `jazz-next/src/JazzNext/Compiler/Runtime/Primitives.hs`: argument validation
  and primitive dispatch.
- `jazz-next/src/JazzNext/Compiler/Runtime/Semantics.hs`: deterministic value
  rendering and representation helpers shared with primitives.
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`: runtime builtin execution and
  observation integration.
- `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`: unchanged consumer of
  the canonical catalog; its existing tests prove new bridges are validated.

### Jazz-authored library

- Modify: `jazz-next/jazz/stdlib/List.jz`, `Maybe.jz`, `Result.jz`, `Text.jz`,
  and `Char.jz`.
- Create: `jazz-next/jazz/stdlib/NonEmpty.jz`, `Dictionary.jz`, `Map.jz`,
  `Set.jz`, and `Queue.jz`.
- `jazz-next/jazz/compiler/Lexer.jz`: dogfood `Dictionary` and list search after
  library behavior is independently green.

### Tests, corpus, editor, and docs

- Extend parser/module suites under
  `jazz-next/test/JazzNext/Compiler/{Parser,Modules}`.
- Create a Cabal-registered stdlib suite under
  `jazz-next/test/JazzNext/Compiler/Stdlib/` with focused foundation,
  collection, and text test modules.
- Extend `jazz-next/test/JazzNext/TestSource.hs` with one shared checked-in
  stdlib/compiler source lookup instead of adding more per-suite filename
  tables.
- Add external fixtures under `jazz-next/test/fixtures/stdlib/`.
- Add production-shaped programs under `jazz-next/programs/` and register them
  in `jazz-next/programs/corpus.json`.
- Extend the TextMate grammar and representative fixture under
  `jazz-next/editors/vscode-jazz/`.
- Update active specifications, stdlib documentation, performance guidance,
  feature status, and the improvement backlog only as their corresponding
  behavior lands.

---

### Task 1: Parse and Lower Grouped Constructor Exports

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/ModuleExports.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleGraph.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs`

**Interfaces:**

- Preserve ordinary bare and namespace-qualified selector behavior.
- Add structured type selector states for abstract, all constructors, and a
  non-empty selected constructor list.
- Retain exact spans for the grouped type and every selected constructor until
  resolver validation.
- Keep `ModuleExportInventory` flat and unchanged in meaning.

- [ ] **Step 1: Add parser and lowering regression tests.**

  Cover `type T`, `type T(..)`, one and many selected constructors, multiple
  grouped selectors, compatibility selectors, and source-qualified spans.
  Add negative cases for `type T()`, malformed dot counts, missing commas,
  missing closing parentheses, non-identifier entries, and duplicate names
  inside one group. Assert `E0001` plus the primary span, not only message text.

- [ ] **Step 2: Run the focused suites and confirm the new cases fail.**

  Run:

  ```text
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test module-import-parser-spec module-exports-spec --project-dir=jazz-next --test-show-details=failures
  ```

  Expected: existing cases pass; grouped-form cases fail because the source
  model and grammar do not yet represent constructor groups.

- [ ] **Step 3: Implement the structured parser/lowering model.**

  Parse `..` only in the grouped type-export context, reject empty and duplicate
  selected groups at the syntax boundary, and preserve located group members.
  Update rendering/equality/order support used by duplicate diagnostics without
  expanding groups into flat inventory entries yet.

- [ ] **Step 4: Re-run the focused suites.**

  Expected: all parser and selector-model cases pass; resolver behavior remains
  unchanged until Task 2.

- [ ] **Step 5: Commit the parser milestone.**

  Commit message: `feat: parse grouped constructor exports`

---

### Task 2: Validate Constructor Ownership and Publish One Flat Inventory

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/ModuleExports.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Modules/Loader/VisibilityTests.hs`
- Test entrypoint: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`
- Modify: `jazz-next/editors/vscode-jazz/syntaxes/jazz.tmLanguage.json`
- Modify: `jazz-next/editors/vscode-jazz/fixtures/representative.jz`
- Modify: `docs/spec/modules/06-explicit-export-lists.md`

**Interfaces:**

- Expand a validated grouped type selector into one `TypeNamespace` entry plus
  the selected owned `ConstructorNamespace` entries.
- Continue feeding the resulting inventory to `ModuleCompiler`,
  `ModuleInterface`, and `ModuleRuntime` without a second group-aware path.
- Use `E4015` for unknown types, unknown constructors, wrong-owner constructors,
  wrong namespaces, and imported-only declarations.
- Accept overlapping selectors and deduplicate through the inventory set.

- [ ] **Step 1: Add resolver and publication failures first.**

  Add cases for abstract, all, and selected exports; a constructor belonging to
  another local type; an imported constructor; an unknown constructor; an
  unknown type; overlapping selector deduplication; local access to hidden
  constructors; downstream expression and pattern visibility; interface
  filtering; and runtime publication. Assert the exact offending selector span
  and stable code.

- [ ] **Step 2: Run the focused module suites and confirm the new cases fail.**

  Run:

  ```text
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test module-resolution-spec module-pipeline-contract-spec loader-spec --project-dir=jazz-next --test-show-details=failures
  ```

- [ ] **Step 3: Implement ownership validation and expansion.**

  Build constructor ownership from local data declarations, validate groups
  before public selection, and expand only validated groups. Keep private local
  compiler/runtime bindings available within the defining module. Do not add
  re-export behavior or import-side grouping.

- [ ] **Step 4: Update syntax highlighting and the active module spec.**

  Scope the grouped type name and selected constructors distinctly in module
  headers. Add representative abstract/all/selected examples. Remove
  constructor-group shorthand from the active spec's non-goals while retaining
  all unrelated exclusions.

- [ ] **Step 5: Re-run parser, module, and repository-audit coverage.**

  Run the Task 1 and Task 2 suites plus `repository-audit-spec`. Expected: all
  pass and existing explicit export behavior remains compatible.

- [ ] **Step 6: Commit the export-semantics milestone.**

  Commit message: `feat: resolve grouped constructor exports`

---

### Task 3: Give Prelude Capabilities Useful Operations

**Files:**

- Modify: `jazz-next/jazz/stdlib/Prelude.jz`
- Modify: `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Primitives.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Semantics.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/CapabilitiesTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RenderingTests.hs`
- Test entrypoint: `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

**Interfaces:**

- Add `Ordering` with `LT`, `EQ`, and `GT` to the bundled prelude.
- Add `Ord::compare`, `Showable::show`, and `Default::defaultValue` with method
  bodies for every instance promised by the design.
- Give private primitive rendering one canonical builtin symbol, a polymorphic
  value-to-`Text` type, arity one, and deterministic runtime behavior.
- Implement numeric ordering in Jazz from existing comparison operators;
  implement `Char` ordering from scalar values and `Text` ordering
  lexicographically from existing text traversal.

- [ ] **Step 1: Add capability and bridge regression tests.**

  Cover catalog ownership/name/arity/type agreement, missing-bridge rejection,
  every existing numeric `Ord` target, `Char` and `Text` ordering, `show` output
  for numbers/booleans/chars/text including escapes, and `defaultValue` for all
  promised primitive targets. Preserve missing-impl diagnostics for incomplete
  custom capabilities.

- [ ] **Step 2: Run focused tests and confirm the new contracts fail.**

  Run:

  ```text
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test builtin-catalog-spec prelude-loading-spec runtime-semantics-spec --project-dir=jazz-next --test-show-details=failures
  ```

- [ ] **Step 3: Extend the canonical builtin pipeline.**

  Add the private renderer consistently across catalog lookup, type inference,
  primitive dispatch, runtime evaluation, observation accounting, and prelude
  bridge validation. Reuse the existing deterministic runtime renderer rather
  than introducing a second formatting implementation.

- [ ] **Step 4: Implement the prelude datatypes, methods, and instances.**

  Keep method bodies explicit, do not add superclass/default-method machinery,
  and do not expand unrelated numeric marker classes. Ensure the prelude
  remains valid in kernel-only and bundled modes.

- [ ] **Step 5: Re-run focused tests and the profiling build.**

  Run the focused suites, then:

  ```text
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal build all --project-dir=jazz-next -fprofiling-workflow
  ```

  Expected: capability behavior is green and the new runtime path remains
  compatible with the existing profiling build.

- [ ] **Step 6: Commit the capability milestone.**

  Commit message: `feat: add useful prelude capabilities`

---

### Task 4: Expand List, Maybe, Result, and NonEmpty

**Files:**

- Modify: `jazz-next/jazz/stdlib/List.jz`
- Modify: `jazz-next/jazz/stdlib/Maybe.jz`
- Modify: `jazz-next/jazz/stdlib/Result.jz`
- Create: `jazz-next/jazz/stdlib/NonEmpty.jz`
- Modify: `jazz-next/test/JazzNext/TestSource.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Stdlib/Shared.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Stdlib/FoundationsTests.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Stdlib/StdlibSpec.hs`
- Create fixtures: `jazz-next/test/fixtures/stdlib/foundations/*.jz`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**

- Export `Maybe` and `Result` through grouped all-constructor syntax.
- Export `NonEmpty(..)` because pattern matching on its guaranteed head/tail
  representation is part of the public API.
- Provide every foundational function named in the approved design, preserving
  safe `Maybe` outcomes, stable sorting, clamped counts, and module-prefixed
  names.
- Use `Eq::equals` for equality-based functions and `Ord::compare` for ordered
  functions.
- Preserve first occurrence order for distinct values, group only adjacent
  related values, include the initial accumulator in scans, and have
  `maybeFromList` return the first item.

- [ ] **Step 1: Register the stdlib suite and write external behavior fixtures.**

  Add focused cases for every public foundation operation, all empty/boundary
  outcomes, stable duplicate sorting, generic values, constrained equality and
  ordering, partial application, cross-module imports, and `NonEmpty`
  conversions. Add a 50,000-element generated traversal case to the Haskell
  test module rather than checking in an enormous source file.

- [ ] **Step 2: Run `stdlib-spec` and confirm it fails before implementation.**

  Run:

  ```text
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test stdlib-spec --project-dir=jazz-next --test-show-details=failures
  ```

  Expected: the new suite is discoverable and fails on missing exports/modules.

- [ ] **Step 3: Centralize checked-in Jazz source lookup.**

  Extend `JazzNext.TestSource` and the stdlib shared harness so current and
  future stdlib modules resolve from the checked-in source root without another
  hard-coded filename case table. Preserve path containment and missing-file
  diagnostics.

- [ ] **Step 4: Implement the foundational modules in dependency order.**

  Land `Maybe`/`Result` helpers, stack-safe list foundations and higher-order
  operations, then `NonEmpty`. Build right folds and stable sorting from the
  lower-level Jazz operations rather than new Haskell builtins. Keep the legacy
  unprefixed prelude functions compatible.

- [ ] **Step 5: Run foundation, capability, lexer-parity, and audit suites.**

  Expected: all new APIs pass, large traversals do not grow the host stack, and
  expanding shipped modules does not break source formatting or the hosted
  lexer.

- [ ] **Step 6: Commit the foundational stdlib milestone.**

  Commit message: `feat: expand Jazz foundation libraries`

---

### Task 5: Add Dictionary and Queue

**Files:**

- Create: `jazz-next/jazz/stdlib/Dictionary.jz`
- Create: `jazz-next/jazz/stdlib/Queue.jz`
- Create: `jazz-next/test/JazzNext/Compiler/Stdlib/LinearCollectionsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Stdlib/StdlibSpec.hs`
- Create fixtures: `jazz-next/test/fixtures/stdlib/linear-collections/*.jz`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**

- Keep both constructors private by exporting only their type identities and
  prefixed public functions.
- `Dictionary` uses equality, preserves first insertion position, and keeps the
  last value for duplicate inputs.
- Reinserting a removed dictionary key places it at the end of the current
  insertion order.
- `dictionaryReplace` returns `Nothing` for an absent key;
  `dictionaryUpdate` maps the current optional value to an optional replacement.
- `Queue` uses front/reversed-rear lists and size, with safe optional peek and
  dequeue results.

- [ ] **Step 1: Add deterministic model and persistence tests.**

  Cover all public functions, duplicate keys, replace/update absence,
  insertion-order-preserving replacement/removal/reinsertion, FIFO order,
  normalization at an empty front, enqueue-all, old-version reuse, generic
  payloads, and large sequential workloads. Compare deterministic generated
  operation traces with simple Haskell list models.

- [ ] **Step 2: Run `stdlib-spec` and confirm the new collection cases fail.**

- [ ] **Step 3: Implement `Dictionary` in Jazz.**

  Use an opaque association-list representation, rebuild only the required
  prefix, reuse list helpers, and do not add a map/hash runtime primitive.

- [ ] **Step 4: Implement `Queue` in Jazz.**

  Normalize only when the front is empty, preserve the documented size and
  ordering invariants, and avoid claiming worst-case real-time behavior.

- [ ] **Step 5: Re-run stdlib, module-publication, and repository-audit suites.**

  Include an explicit check that downstream modules cannot name the private
  collection constructors but can use values returned by the public API.

- [ ] **Step 6: Commit the linear-collections milestone.**

  Commit message: `feat: add persistent Dictionary and Queue`

---

### Task 6: Add Persistent AVL Map and Set

**Files:**

- Create: `jazz-next/jazz/stdlib/Map.jz`
- Create: `jazz-next/jazz/stdlib/Set.jz`
- Create: `jazz-next/test/JazzNext/Compiler/Stdlib/OrderedCollectionsTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Stdlib/StdlibSpec.hs`
- Create fixtures: `jazz-next/test/fixtures/stdlib/ordered-collections/*.jz`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**

- Keep AVL node constructors and set wrappers private.
- Use only `Ord::compare` for keys; `EQ` identifies replacement/removal.
- Return ascending order from conversions, keys, folds, and extrema.
- Preserve `O(log n)` lookup/update/removal under a lawful comparison and
  structural sharing of untouched subtrees.
- Implement `Set` on the public/internal `Map(a, ())` abstraction without a
  second balancing algorithm.

- [ ] **Step 1: Add red behavior and model-based tests.**

  Cover every public function, all four AVL rotation shapes, repeated
  replacement, leaf/one-child/two-child/root removal, extrema and pop results,
  ascending traversal, older-version reuse, set algebra, and `setMap`
  deduplication. Use deterministic generated traces compared with
  `Data.Map.Strict` and `Data.Set` reference results.

- [ ] **Step 2: Add an internal invariant test path without widening exports.**

  Keep a private `mapInvariantHolds` helper inside `Map.jz`. Extend the test-only
  stdlib harness to evaluate the target module's full compiled scope and invoke
  that private binding from the retained `scopeResultEnvironment`. Do not alter
  the public export inventory or add a production `mapIsValid` API. Validate
  stored heights, cached size if present, binary-search ordering, and balance
  factors after deterministic generated traces.

- [ ] **Step 3: Run `stdlib-spec` and confirm ordered-collection failures.**

- [ ] **Step 4: Implement `Map` and then `Set` in Jazz.**

  Keep rotations and height maintenance localized, reuse lower-level list and
  `Maybe` APIs, and keep set operations expressed through the map implementation.

- [ ] **Step 5: Run stdlib tests with large ordered workloads.**

  Expected: behavior matches the Haskell models, invariants hold after every
  trace prefix, and no private constructor becomes importable.

- [ ] **Step 6: Commit the ordered-collections milestone.**

  Commit message: `feat: add persistent Jazz Map and Set`

---

### Task 7: Expand Text and Char with Narrow Runtime Bridges

**Files:**

- Modify: `jazz-next/jazz/stdlib/Text.jz`
- Modify: `jazz-next/jazz/stdlib/Char.jz`
- Modify: `jazz-next/jazz/stdlib/Prelude.jz`
- Modify: `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Primitives.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime/Semantics.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Stdlib/TextTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Stdlib/StdlibSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`
- Test entrypoint: `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- Create fixtures: `jazz-next/test/fixtures/stdlib/text/*.jz`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**

- Add private canonical builtins for text concatenation and simple Unicode case
  mapping with catalog/type/runtime agreement and observation accounting.
- Keep `textConcat` and `textJoin` as the public bulk-construction surfaces;
  expose no builder datatype.
- Count Unicode scalar values for indexes and slices.
- Define `textSlice` as start plus scalar count, `textFind` as the first scalar
  index, padding through a caller-supplied `Char`, and joining with delimiters
  only between fragments.
- Preserve all edge semantics in the approved design, including empty
  delimiter splitting, empty-search replacement, non-overlapping matches, and
  CRLF handling.

- [ ] **Step 1: Add primitive and public API failures first.**

  Cover builtin catalog drift, wrong arity/type/runtime values, text-concat
  order and large input, every scalar indexing boundary, Unicode search and
  slicing, empty delimiter and replacement behavior, trim/words/lines,
  padding, simple case conversion, and deterministic repeatability.

- [ ] **Step 2: Run stdlib, builtin, and primitive suites and confirm failures.**

- [ ] **Step 3: Implement the narrow private primitives.**

  Use one allocation for final text concatenation, reuse the locked GHC/text
  Unicode behavior for simple scalar casing, and route invalid runtime values
  through structured diagnostics. Do not move search/split/slice algorithms
  into Haskell.

- [ ] **Step 4: Implement the public Jazz algorithms.**

  Build on `textUncons`, lists, `Maybe`, and `textConcat`; keep traversal
  stack-safe and share common helpers rather than repeating scans.

- [ ] **Step 5: Re-run focused suites, the full stdlib suite, and profiling build.**

- [ ] **Step 6: Commit the text/character milestone.**

  Commit message: `feat: expand Jazz text and character libraries`

---

### Task 8: Dogfood the Library and Add Corpus/Benchmark Evidence

**Files:**

- Modify: `jazz-next/jazz/compiler/Lexer.jz`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzLexerParitySpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`
- Modify: `jazz-next/test/JazzNext/TestSource.hs`
- Create: `jazz-next/programs/word-frequency/`
- Create: `jazz-next/programs/sorted-index/`
- Create: `jazz-next/programs/queue-traversal/`
- Create: `jazz-next/programs/text-processing/`
- Create: `jazz-next/programs/collection-boundaries/`
- Modify: `jazz-next/programs/corpus.json`
- Modify: `jazz-next/programs/README.md`
- Modify: `jazz-next/PERFORMANCE.md`

**Interfaces:**

- Replace the lexer's nested keyword classification with a checked-in
  `Dictionary(Text, CanonicalTokenKind)` value and `dictionaryLookup`.
- Replace the operator-character conditional chain with `listContains`.
- Preserve exact canonical lexer output, diagnostics, deterministic repetition,
  and large-input stack safety.
- Register production-shaped programs with expected output, feature tags,
  benchmark groups, and deterministic runtime budgets.

- [ ] **Step 1: Strengthen lexer and corpus tests before refactoring.**

  Ensure parity coverage includes every keyword and operator character plus
  unknown identifiers/characters. Add manifest validation cases for any new
  feature tags. Add expected-output fixtures for all five new corpus programs.

- [ ] **Step 2: Refactor the Jazz lexer through public stdlib APIs.**

  Add only the required stdlib imports and update the shared source lookup so
  the compiler module resolves transitive library dependencies without another
  repeated table. Do not expose stdlib internals to the compiler module.

- [ ] **Step 3: Run canonical comparison and Jazz lexer parity.**

  Run:

  ```text
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test canonical-lexer-comparison-spec jazz-lexer-parity-spec --project-dir=jazz-next --test-show-details=failures
  ```

- [ ] **Step 4: Register and validate the five corpus programs.**

  Exercise insertion-ordered word frequency, ascending map/set traversal, FIFO
  queue traversal, Unicode text processing, and private-constructor module
  boundaries. Reuse shipped stdlib modules through the existing corpus module
  root rather than copying their implementations into program directories.

- [ ] **Step 5: Establish deterministic budgets from observed runs.**

  Run the corpus with runtime statistics, record actual values locally, choose
  reviewable headroom, and explain any material increase in comments or the PR
  evidence rather than treating old budgets as immutable performance limits.

- [ ] **Step 6: Run corpus and benchmark smoke gates.**

  Run:

  ```text
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test program-corpus-spec benchmark-stage-spec --project-dir=jazz-next --test-show-details=failures
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal bench jazz-next-bench --project-dir=jazz-next --benchmark-options='--jazz-smoke'
  ```

- [ ] **Step 7: Update performance documentation and commit.**

  Document the new focused cases, semantic-stat interpretation, expected
  collection complexity, and the fact that recorded machine results remain
  evidence rather than committed universal thresholds.

  Commit message: `feat: dogfood and benchmark Jazz collections`

---

### Task 9: Full Verification, Active Documentation, and Batch Closeout

**Files:**

- Create: `jazz-next/jazz/stdlib/README.md`
- Modify: `jazz-next/README.md`
- Modify: `docs/feature-status.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/jazz-improvement-backlog.md`
- Verify without changing the independent target: `docs/execution/queue.md`

**Interfaces:**

- Document every public module, datatype abstraction boundary, operation
  family, edge behavior, ordering guarantee, and complexity promise.
- Mark Batch 5 complete only after the complete code/test/benchmark gate passes.
- Preserve historical plans as evidence and keep the bootstrap queue's parser
  curation target unchanged.

- [ ] **Step 1: Run the complete compiler and repository gate.**

  Run from the repository root:

  ```text
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test all --project-dir=jazz-next --test-show-details=failures
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal build all --project-dir=jazz-next -fprofiling-workflow
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal bench jazz-next-bench --project-dir=jazz-next --benchmark-options='--jazz-smoke'
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: every Cabal test suite passes, profiling build succeeds, benchmark
  smoke executes every group, queue/docs checks pass, and the diff is clean.

- [ ] **Step 2: Capture same-machine benchmark evidence.**

  Run focused recorded benchmarks for the new fast collection/text cases with
  an explicit local environment label. Keep generated artifacts ignored. Note
  meaningful differences and their semantic explanation for the eventual PR;
  do not manufacture a pass/fail percentage.

- [ ] **Step 3: Write the active stdlib and language documentation.**

  Add module/API/complexity tables to the stdlib README, update active language
  and feature status for grouped exports and library coverage, and repair all
  current links/path references touched by the batch.

- [ ] **Step 4: Close the improvement backlog entry.**

  Mark Batch 5 completed with links to the approved design, this plan, the
  stdlib reference, module export spec, and performance evidence commands.
  Do not edit the live bootstrap queue except for a source-backed date/link
  correction required by validation.

- [ ] **Step 5: Re-run docs/queue checks and targeted package audits.**

  Run `repository-audit-spec`, `program-corpus-spec`, both shell checks, and
  `git diff --check` after documentation changes.

- [ ] **Step 6: Review scope and commit closeout.**

  Confirm `git status --short` contains no generated benchmark/profile files
  and no modifications under `jazz-hs/` or `jazz2/`.

  Commit message: `docs: close broad Jazz stdlib batch`

---

## Execution Checkpoints

- Stop after any red test that fails for a reason unrelated to the intended
  missing behavior; diagnose the existing regression before implementing.
- If a Jazz-authored API exposes a real missing type-solver/runtime capability,
  add the smallest compiler support consistent with the approved semantics and
  regression-test the invariant. Do not weaken the public signature or move the
  whole algorithm into Haskell as a shortcut.
- If an internal persistent-collection invariant cannot be observed without
  widening the public API, improve the compiled-module test harness rather than
  exporting representation details.
- Keep the working tree free of generated benchmark and profile artifacts at
  every commit boundary.
- Execute this plan inline in the current worktree; the tasks share source and
  semantic contracts and are not suitable for independent concurrent edits.
