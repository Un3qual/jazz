---
id: JN-GENERIC-CAPABILITIES-LIBRARY-001
status: complete
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - CHANGELOG.md
  - docs/language/types-and-signatures.md
  - docs/project/roadmap.md
  - docs/project/status.md
  - docs/standard-library/char.md
  - docs/standard-library/dictionary.md
  - docs/standard-library/io.md
  - docs/standard-library/list.md
  - docs/standard-library/map.md
  - docs/standard-library/maybe.md
  - docs/standard-library/nonempty.md
  - docs/standard-library/overview.md
  - docs/standard-library/prelude.md
  - docs/standard-library/queue.md
  - docs/standard-library/reduce.md
  - docs/standard-library/result.md
  - docs/standard-library/set.md
  - docs/standard-library/text.md
  - jazz.cabal
  - jazz/compiler/CoreLower.jz
  - jazz/compiler/CoreTypes.jz
  - jazz/compiler/Lexer.jz
  - jazz/compiler/ParserContext.jz
  - jazz/compiler/ParserCore.jz
  - jazz/compiler/ParserDeclaration.jz
  - jazz/compiler/ParserExpression.jz
  - jazz/compiler/ParserOperator.jz
  - jazz/compiler/ParserPattern.jz
  - jazz/compiler/ParserProgram.jz
  - jazz/compiler/ParserSignature.jz
  - jazz/compiler/ParserToken.jz
  - jazz/compiler/ParserTypes.jz
  - jazz/stdlib/Char.jz
  - jazz/stdlib/Dictionary.jz
  - jazz/stdlib/IO.jz
  - jazz/stdlib/IOError.jz
  - jazz/stdlib/List.jz
  - jazz/stdlib/Map.jz
  - jazz/stdlib/Maybe.jz
  - jazz/stdlib/NonEmpty.jz
  - jazz/stdlib/Prelude.jz
  - jazz/stdlib/Queue.jz
  - jazz/stdlib/README.md
  - jazz/stdlib/Reduce.jz
  - jazz/stdlib/Result.jz
  - jazz/stdlib/Set.jz
  - jazz/stdlib/Text.jz
  - programs/capability-workflow/Workflow.jz
  - programs/collection-boundaries/Collections.jz
  - programs/collection-boundaries/Main.jz
  - programs/dependency-planner/Graph.jz
  - programs/expression-evaluator/Expression.jz
  - programs/fannkuch/Fannkuch.jz
  - programs/identifier-classifier/Main.jz
  - programs/merge-sort/MergeSort.jz
  - programs/mini-frontend/Analysis.jz
  - programs/mini-frontend/Evaluation.jz
  - programs/mini-frontend/Token.jz
  - programs/n-queens/Queens.jz
  - programs/prime-sieve/Sieve.jz
  - programs/queue-traversal/Main.jz
  - programs/queue-traversal/Traversal.jz
  - programs/sorted-index/Index.jz
  - programs/text-processing/Main.jz
  - programs/word-frequency/Main.jz
  - rfcs/accepted/0019-generic-capabilities-and-library-names.md
  - scripts/check-clarification-specs.sh
  - scripts/check-stdlib-api-docs.py
  - scripts/public-doc-fragments.tsv
  - scripts/test-check-public-docs.py
  - scripts/test-check-stdlib-api-docs.py
  - scripts/test-weeder-policy.sh
  - src/Jazz/Compiler/BuiltinCatalog.hs
  - src/Jazz/Compiler/BundledPrelude.hs
  - src/Jazz/Compiler/Prelude.hs
  - src/Jazz/Compiler/PreludeContract.hs
  - src/Jazz/Compiler/Runtime/Primitives.hs
  - src/Jazz/Compiler/TypeInference.hs
  - src/Jazz/Compiler/TypeInference/Capabilities.hs
  - src/Jazz/Compiler/TypeInference/Scope.hs
  - test/Jazz/CLI/CLISpec.hs
  - test/Jazz/Compiler/Bootstrap/CanonicalCoreComparisonSpec.hs
  - test/Jazz/Compiler/Bootstrap/JazzCoreModulesCorpusClosureSpec.hs
  - test/Jazz/Compiler/Bootstrap/JazzCoreParity.hs
  - test/Jazz/Compiler/Bootstrap/JazzCoreSignaturesDeclarationsOperatorsSpec.hs
  - test/Jazz/Compiler/Bootstrap/JazzLexerParitySpec.hs
  - test/Jazz/Compiler/Bootstrap/JazzParserComponentSpec.hs
  - test/Jazz/Compiler/Bootstrap/JazzParserOperatorsFullParitySpec.hs
  - test/Jazz/Compiler/Bootstrap/JazzParserScale.hs
  - test/Jazz/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs
  - test/Jazz/Compiler/Bootstrap/ParserCoreSpec.hs
  - test/Jazz/Compiler/Diagnostics/StructuredErrorDiagnosticsSpec.hs
  - test/Jazz/Compiler/HaskellTypeclassContractsSpec.hs
  - test/Jazz/Compiler/Modules/Loader/AliasClassTests.hs
  - test/Jazz/Compiler/Modules/Loader/CapabilitiesTests.hs
  - test/Jazz/Compiler/Modules/Loader/VisibilityTests.hs
  - test/Jazz/Compiler/Modules/LoaderSpec.hs
  - test/Jazz/Compiler/Modules/ModuleExportsSpec.hs
  - test/Jazz/Compiler/Modules/ModulePipelineContractSpec.hs
  - test/Jazz/Compiler/Modules/ModuleResolutionSpec.hs
  - test/Jazz/Compiler/Modules/PreludeLoadingSpec.hs
  - test/Jazz/Compiler/Parser/FixtureCorpus.hs
  - test/Jazz/Compiler/Parser/Foundation/ExpressionsTests.hs
  - test/Jazz/Compiler/Parser/Foundation/InvalidSyntaxTests.hs
  - test/Jazz/Compiler/Parser/Foundation/ModulesTests.hs
  - test/Jazz/Compiler/Parser/Foundation/SignaturesTests.hs
  - test/Jazz/Compiler/Runtime/Observation/ProfileTests.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/BasicsTests.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/ConstraintsTests.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/DiagnosticsTests.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/GeneralizationTests.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/RecursionTests.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/Shared.hs
  - test/Jazz/Compiler/Semantics/BuiltinCatalogSpec.hs
  - test/Jazz/Compiler/Semantics/PrimitiveSemantics/ScalarCollection.hs
  - test/Jazz/Compiler/Semantics/RebindingWarningSpec.hs
  - test/Jazz/Compiler/Semantics/Runtime/CapabilitiesTests.hs
  - test/Jazz/Compiler/Semantics/Runtime/RenderingTests.hs
  - test/Jazz/Compiler/Stdlib/FoundationsTests.hs
  - test/Jazz/Compiler/Stdlib/GenericCapabilitiesTests.hs
  - test/Jazz/Compiler/Stdlib/LinearCollectionsTests.hs
  - test/Jazz/Compiler/Stdlib/OrderedCollectionsTests.hs
  - test/Jazz/Compiler/Stdlib/StdlibSpec.hs
  - test/Jazz/Repository/AuditSpec.hs
  - test/fixtures/stdlib/foundations/ListBoundaries.jz
  - test/fixtures/stdlib/foundations/ListNormalize.jz
  - test/fixtures/stdlib/foundations/ListPartialStable.jz
  - test/fixtures/stdlib/foundations/ListShape.jz
  - test/fixtures/stdlib/foundations/ListTransform.jz
  - test/fixtures/stdlib/foundations/MaybeResult.jz
  - test/fixtures/stdlib/foundations/MaybeResultBranches.jz
  - test/fixtures/stdlib/foundations/NonEmpty.jz
  - test/fixtures/stdlib/generic/GenericLaws.jz
  - test/fixtures/stdlib/generic/GenericLibrary.jz
  - test/fixtures/stdlib/linear-collections/Dictionary.jz
  - test/fixtures/stdlib/linear-collections/Queue.jz
  - test/fixtures/stdlib/ordered-collections/Map.jz
  - test/fixtures/stdlib/ordered-collections/MapExtrema.jz
  - test/fixtures/stdlib/ordered-collections/MapPersistence.jz
  - test/fixtures/stdlib/ordered-collections/MapShapes.jz
  - test/fixtures/stdlib/ordered-collections/MapTraversal.jz
  - test/fixtures/stdlib/ordered-collections/Set.jz
  - test/fixtures/stdlib/text/CharCase.jz
  - test/fixtures/stdlib/text/Core.jz
  - test/fixtures/stdlib/text/LargeConcat.jz
  - test/fixtures/stdlib/text/Search.jz
  - test/fixtures/stdlib/text/SplitCleanup.jz
  - website/scripts/check-built-type-links.mjs
  - website/scripts/jazz-type-links.mjs
  - website/scripts/test-built-search-index.mjs
  - website/scripts/test-experience.mjs
  - website/scripts/test-jazz-type-links.mjs
  - website/scripts/test-pagefind-search-model.mjs
  - website/sidebars.ts
verification:
  - cabal build all --jobs=1
  - cabal test all --jobs=1 --test-show-details=failures
  - cabal test jazz-parser-scale-full-expression-spec jazz-parser-scale-full-declarations-spec jazz-parser-scale-full-control-flow-spec jazz-parser-scale-full-operator-spec -ffull-parser-scale --jobs=1 --test-show-details=failures
  - JAZZ_CABAL_JOBS=1 bash scripts/ci/haskell-quality.sh
  - bash scripts/check-examples.sh --jazz-bin "$(cabal list-bin jazz)"
  - bash scripts/check-docs.sh
  - bash scripts/check-execution-queue.sh
  - git diff --check
deliverable: "Bundled generic capabilities, collection instances, all 183 public renames, Text mapping, Reduce, and migrated consumers and documentation."
last_verified: 2026-09-14
---

# Generic Capabilities Library Migration

Execute inline, without agent fan-out. RFC 0019 and the source plan approve this
scope. The verified core child provides kinds, generic evidence, defaults,
superclasses, ordinary methods, and transitive instance transport.

Use [the approved rename inventory](2026-09-13-stdlib-api-renames.csv) and retain
existing argument orders. The core implementation is committed through
`6970d538`; its complete test, build, full parser-scale, quality, example,
documentation, and queue gates passed before this promotion.

## Implementation

The implementation adds these files with their owning module and registration changes:
`docs/standard-library/reduce.md`, `jazz/stdlib/Reduce.jz`, `test/Jazz/Compiler/Stdlib/GenericCapabilitiesTests.hs`, `test/fixtures/stdlib/generic/GenericLaws.jz`, `test/fixtures/stdlib/generic/GenericLibrary.jz`.

Bundled capabilities, final library names, and consumers

**Files:** `jazz/stdlib/Prelude.jz`, `List.jz`, `Queue.jz`, `Maybe.jz`,
`Result.jz`, `NonEmpty.jz`, `Map.jz`, `Dictionary.jz`, `Set.jz`, `Text.jz`,
new `jazz/stdlib/Reduce.jz`, `src/Jazz/Compiler/BuiltinCatalog.hs`,
`BundledPrelude.hs`, `PreludeContract.hs`, `Prelude.hs`,
`TypeInference/Capabilities.hs`, `TypeInference/Scope.hs`,
`test/Jazz/Compiler/Stdlib/{LinearCollectionsTests,OrderedCollectionsTests,TextTests,FoundationsTests}.hs`,
`test/Jazz/Compiler/Modules/PreludeLoadingSpec.hs`,
`test/Jazz/Compiler/Semantics/BindingSignature/ConstraintsTests.hs`,
`test/Jazz/Compiler/Semantics/PrimitiveSemantics/EqualityOperator.hs`,
`test/Jazz/CLI/CLISpec.hs`,
`test/Jazz/Repository/AuditSpec.hs`, and new `docs/standard-library/reduce.md`.
Also every source/export in `2026-09-13-stdlib-api-renames.csv`,
`jazz/compiler/`, `test/fixtures/stdlib/`, Haskell-embedded Jazz fixtures,
`programs/`, `examples/`, `docs/standard-library/`, public examples/signatures,
`jazz/stdlib/README.md`, `scripts/check-stdlib-api-docs.py`,
`scripts/test-check-stdlib-api-docs.py`, and affected editor grammar owners.

**Interfaces:** Define the five RFC classes in the Prelude. `Mappable(f)` uses
`map :: (a -> b) -> f(a) -> f(b)`; `Reducible(f)` uses ordinary `f(a)` folds.
Collection-owned instances arrive on import through Task 4; list instances
remain in the Prelude. The public list-only builtin map becomes the class
method. Add Text map and retain Set's existing map signature/argument order.
The CSV maps 183 existing exports without changing their argument orders; the
two class renames and new exports are tracked separately. Migrate a module's
public names, instances, consumers, and documentation together, using final
names from the start. Generic Prelude methods remain distinct from specialized
module values. There is no separate second pass to rename newly added adapters.

- [x] Reconcile the CSV with live exports before editing. Rename Eq/Ord to
      Equatable/Comparable with their compiler consumers, fixtures, and public
      documentation. Update the public builtin map binding and hardcoded class
      inventories with the Prelude classes/instances. Keep `Default` separate.
      Migrate `BundledPrelude.renderCapabilityClass` and
      `renderDefaultCapabilityImpl` with the authored Prelude so default CLI and
      driver compilation expose the same renamed capabilities. Update the strict
      equality obligation label used by
      `TypeInference/Scope.addUndeclaredSignatureConstraintErrors` for declared
      constraint matching and diagnostics, preserving builtin structural `==`.
      Reuse the existing Prelude-loading, signed-equality, primitive-equality,
      and CLI cases listed above to check the renamed paths.
- [x] Work through modules in dependency order, keeping related modules together
      when needed for a compiling milestone. For each group, add behavioral
      fixtures with final names, implement its instances and renames, and update
      consumers and API docs before committing. Resolve references by module
      ownership; do not globally replace common names such as map or empty.
      Use `import List as List` and equivalent qualification to avoid collisions.

- [x] In module fixtures, use `import Queue as Queue.`, `import Maybe as Maybe.`,
      `import Text as Text.`, and `import Set as Set.` with this helper:

  ```jazz
  convert = \(change, values) -> map change values.
  listResult = convert (\(value) -> value == 1) [1, 2].
  queueResult = convert (\(value) -> value == 1) (Queue::fromList [1, 2]).
  maybeResult = convert (\(value) -> value == 1) (Maybe::Just 1).
  unchanged = Text::map (\(character) -> character) "abc".
  unique = Set::map (Set::fromList [1, 2]) (\(value) -> 0).
  ```

  Expect `[True, False]`, a Queue with those values, `Just True`, `"abc"`, and a
  singleton Set. No helper signature or destination-collection annotation is
  required. Add Result error preservation, NonEmpty, and Map/Dictionary key
  preservation cases, plus a collection absent from the stdlib.

- [x] Cover Text-to-integer mapping explicitly through `Text::toChars` and List
      map, and Set-to-List through `Set::toList`. Reject a non-Char Text callback,
      Set output without Comparable evidence, generic Mappable(Text/Set) uses,
      and an output annotation that changes a List map into a Queue. Verify Set
      remains Reducible without element ordering evidence; Text reduction goes
      through its character conversion.
- [x] Run `stdlib-spec` and `binding-signature-coherence-spec` to establish
      failures in the new class and Text-map behavior before implementation.
- [x] Implement ordinary Jazz instances using existing traversals. Add the new
      function-first Text map with type `(Char -> Char) -> Text -> Text` and
      Unicode scalar coverage. Do not create a Mapping module or Empty class.
      Text implementation may use `toChars`, `Mappable::map`, and `fromChars`,
      with the existing linear cost and no new kernel operation. Qualify the
      generic map reference so Text's local `map` does not shadow it.
- [x] Implement collection equality through element `Equatable` methods, not
      structural `==`. Queue compares FIFO contents using existing `toList`
      and list equality; do not change its representation or normalization.
      Check queues built with `fromList [1, 2]` and `enqueue (fromList [1]) 2`
      compare equal, and use a custom element equality to catch structural
      fallback across the listed generic equality instances. Implement ordinary
      Prelude tuple instances for pairs and triples only, with one prerequisite
      per component. Preserve existing builtin tuple equality at every supported
      arity; add no variadic instances, generator, or deriving mechanism.
- [x] Implement the RFC's Reducible and Combinable families while migrating their
      owning modules. Keep existing empty values. Check mapping identity/composition,
      FIFO/key order, and empty/NonEmpty behavior.
- [x] Keep `Reduce.jz` as a small explicit-import module after Maybe is available.
      Implement `reduce :: @{Reducible(f)}: (a -> a -> a) -> f(a) -> Maybe(a)`
      once using `foldLeft` with a Maybe accumulator. Nothing takes the first
      element; Just combines the accumulator with the next element. Do not add
      intermediate List conversion, a runtime primitive, or another capability.
      Test empty and singleton inputs plus a non-associative callback that shows
      left-fold order on List and Queue through one generic helper. Register the
      module and its documentation in the authored module/API inventories.
- [x] Run `stdlib-spec`, `prelude-loading-spec`, `builtin-catalog-spec`,
      `binding-signature-coherence-spec`, `primitive-semantics-spec`, `cli-spec`,
      `runtime-semantics-spec`, and `loader-spec`.
      Update public capability/stdlib documentation and commit.

- [x] Run `stdlib-spec`, `repository-audit-spec`, the complete retained hosted
      parser/core suites, and the API-doc checker. Assert old public prefix names
      are absent from active exports and consumer code, allowing historical records
      and migration documentation. Add no compiler workaround unless a focused
      source case demonstrates a real required compatibility issue.
- [x] Run the complete frontmatter verification commands in pinned shells.
      Record each command/result and any explicit new waiver. This includes
      full parser-scale coverage after migrating the library used by the hosted
      parser. No performance claim follows from functional tests.
- [x] Review the full RFC acceptance matrix against observed behavior. Update
      shipped status and public API docs, close the library queue child, refresh
      curation/blocker state, and commit the completed migration.

## Implementation and verification receipt

The library implementation is committed through `99a15625`. Verification
completed on 2026-09-14. All frontmatter commands passed, including the complete compiled test suite, all four
full parser-scale executions, and both fresh Weeder graphs. No parser-scale or
runtime-budget waiver was used. Examples, documentation, queue, and whitespace
checks passed. The website production build, search, type links, navigation,
and boundary checks also passed.

All 183 CSV signatures retain argument order and return contracts after class
renaming and type qualification. The acceptance fixtures cover all Mappable
families, custom collections and element equality, FIFO/key/insertion fold
orders, empty inputs, Text/Set restrictions, safe reduction, and combination.
The authored Prelude matches the generated bundled source exactly. Core
inference, evidence, module transport, defaults, and superclass coverage remain
in the completed core child. RFC 0019 is complete.

Full parser-scale observations, with the existing budgets unchanged:

| Family       | Evaluator transitions | Applications | Maximum continuation depth | Maximum capture width |
| ------------ | --------------------- | ------------ | -------------------------- | --------------------- |
| expression   | 21,851,788            | 2,630,851    | 1,061                      | 41                    |
| declarations | 9,654,992             | 1,152,167    | 1,074                      | 35                    |
| control-flow | 42,053,435            | 5,080,620    | 1,096                      | 41                    |
| operator     | 49,354,312            | 5,939,770    | 1,116                      | 41                    |
