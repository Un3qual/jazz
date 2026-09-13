---
id: JN-GENERIC-CAPABILITIES-CORE-001
status: blocked
priority: P1
size: L
kind: impl
autonomous_ready: no
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/TypeRepresentation.hs
  - src/Jazz/Compiler/SemanticDeclarations.hs
  - src/Jazz/Compiler/Parser/CapabilityDeclaration.hs
  - src/Jazz/Compiler/TypeInference/Capabilities.hs
  - src/Jazz/Compiler/ModuleInterface.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/ConstraintsTests.hs
verification:
  - cabal build all --jobs=1
  - cabal test all --jobs=1 --test-show-details=failures
  - JAZZ_CABAL_JOBS=1 bash scripts/ci/haskell-quality.sh
  - bash scripts/check-examples.sh --jazz-bin "$(cabal list-bin jazz)"
  - bash scripts/check-docs.sh
  - bash scripts/check-execution-queue.sh
  - git diff --check
deliverable: "General generic capability dispatch, kind and dependency inference, ordinary method values, superclass/default evidence, and module transport through the analyzed interpreter."
last_verified: 2026-09-13
---

# Generic Capabilities and Library Names Implementation Plan

> For agentic workers: use `superpowers:executing-plans` to implement this plan
> inline, task by task. No agent fan-out is requested for this work.

**Goal:** Implement the approved generic-programming and naming direction,
including Set/Text mapping into a context-selected output, then complete the
standard-library rename and consumer migration.

**Architecture:** Extend the existing shared type representation, declaration
templates, solver, nominal method identities, and analyzed evidence. Class
methods become ordinary value references. The library defines input/output
mapping relations through multi-parameter classes and type dependencies;
the compiler does not know a hardcoded collection matrix.

**Tech stack:** Haskell and Jazz, existing Cabal suites and Python/shell gates,
the repository-pinned Nix development/quality shells.

**Spec:** [RFC 0019](../../rfcs/proposed/0019-generic-capabilities-and-library-names.md).
The naming direction, Set/Text participation, and context-selected output are
approved. The detailed contract is proposed; execution begins after acceptance
of the new syntax, instance/export policy, and additional `Empty` name.

## Global constraints

- Implement only in the active root `src/`, `jazz/`, `app/`, and `test/` owners.
- Public capability names describe behavior. The approved five are
  `Equatable`, `Comparable`, `Mappable`, `Reducible`, and `Combinable`.
- Both Set and Text support generic map, with output inferred from context.
- A concrete map use with no determined output reports ambiguity, not a default
  collection. Text output requires Char; Set output requires Comparable.
- Keep the single analyzed-core interpreter and nominal class/impl identities.
- Preserve numeric widths, builtin structural equality semantics, direct-call
  purity rules, host behavior, and permanent rejection of `trait` syntax.
- Extend retained hosted syntax/lowering conformance for changed grammar;
  broader self-hosting, native work, profiling, and benchmarks remain deferred.
- Use `--jobs=1` for Cabal. Previous full-scale-test waivers were run-specific.
- Update public behavior, consumer fixtures, queue state, and documentation with
  each coherent implementation. Commit verified milestones.
- Do not weaken existing checks, add same-name runtime fallback, or enumerate
  concrete element types as a substitute for generic implementations.

## Promotion and delivery

After RFC acceptance, move it to `rfcs/accepted/`, update the RFC index and
this link, set this plan to `status: ready` and `autonomous_ready: yes`, and
promote the matching curation candidate with exactly this ordered target and
verification list. The first queue child covers Tasks 1-4 as one complete
compiler deliverable; do not close it at parser-only support.

Tasks 5-6 are the dependent library migration, child
`JN-GENERIC-CAPABILITIES-LIBRARY-001`. At core closeout, create its ready plan
from those tasks, naming the concrete stdlib, catalog, and test files. Preserve
the approved scope and continue to that milestone without reopening it for
permission. The separate child is an integration/verification boundary, not a
request to leave the rename undone.

## Implementation

### Task 1: Kinded applications and generic declaration templates

**Files:** `src/Jazz/Compiler/TypeRepresentation.hs`,
`SemanticDeclarations.hs`, `AST.hs`, `Parser/AST.hs`, `Parser/Signature.hs`,
`Parser/CapabilityDeclaration.hs`, `Parser/Lower.hs`, `SignatureRendering.hs`,
`TypeInference/Signature.hs`, `TypeInference/Solver.hs`,
`jazz/compiler/ParserSignature.jz`, `jazz/compiler/ParserDeclaration.jz`, and
the canonical/hosted adapters that consume those declarations.

**Interfaces:** Replace the type-name-only head of `TypeApplication` with a
recursive type head. The shared representation owns `Kind = TypeKind |
FunctionKind Kind Kind`; inference owns fresh kind variables and substitutions.
Generalize `ClassMethodType` from one parameter to ordered class parameters,
independent method quantifiers, prerequisites, and the method type. One checked
implementation template carries `ImplId`, ordered quantifiers/kinds, all head
arguments, prerequisites, and method identities, replacing the concrete-only
catalog rather than adding a competing generic catalog.

- [ ] In parser/signature and binding suites, add focused source cases:

  ```jazz
  data Wrapped f a = Wrapped f(a).
  keep :: Wrapped(List, Int) -> Wrapped(List, Int).
  keep = \(value) -> value.

  class Transforming(f) {
    transform :: (a -> b) -> f(a) -> f(b).
  }.
  ```

  Reject `Wrapped(Int, Int)`, applying Int as a constructor, excessive Result
  arguments, and kind-occurs-check cycles. Accept partial `Result(error)` and
  normalize `[a]` and `List(a)` identically. Check error locations as behavior,
  not serialized private representation strings.

- [ ] Run `cabal test parser-foundation-spec declaration-parser-spec
binding-signature-coherence-spec signature-rendering-spec --jobs=1
--test-show-details=failures`; confirm the new behavior fails on the baseline.
- [ ] Extend parsing/lowering for `@{...}:` declaration contexts, optional
      `determines (...)`, and class default bodies. Check class arity after kind
      inference rather than retaining the one-target guard in `ImplChecking.hs`.
      Parse arbitrary type applications and normalize them through the existing
      shared owner; substitutions and renderers must agree on one semantic shape.
- [ ] Extend hosted declaration/type encodings and complete all exhaustive
      adapters; retain structural differential checks for the changed syntax.
      Run the focused suites plus `canonical-parser-comparison-spec`,
      `canonical-core-comparison-spec`, and
      `jazz-parser-types-declarations-modules-spec`. Commit the coherent type and
      declaration representation migration once all components build.

### Task 2: Generic resolution, type dependencies, and runtime evidence

**Files:** `src/Jazz/Compiler/CapabilityFacts.hs`, `SemanticDeclarations.hs`,
`TypeInference/Capabilities.hs`, `TypeInference/ImplChecking.hs`,
`TypeInference/Solver.hs`, `TypeInference/Instantiation.hs`,
`TypeInference/State.hs`, `TypeInference/Analyzed.hs`, `CoreIdentity.hs`,
`SemanticFacts.hs`, `Runtime/Types.hs`, `Runtime/ScopePlan.hs`,
`Runtime/Engine.hs`, and `Runtime/Semantics.hs`.

**Interfaces:** The template from Task 1 is the sole declaration environment.
Class obligations contain the class identity and an ordered list of arguments.
Selected evidence identifies an implementation plus substitution and prerequisite
evidence; deferred evidence refers to an enclosing scheme parameter. Extend
existing analyzed facts and method cells with these forms. Preserve the public
driver result APIs.

- [ ] Add compile/run cases using the existing `assertSourceOkWithoutPrelude`
      helper in `BindingSignature/ConstraintsTests.hs` and driver-based runtime
      cases in `Runtime/CapabilitiesTests.hs`. Start with this complete program:

  ```jazz
  class Same(a) { same :: a -> a -> Bool. }.
  impl Same(Int) { same = \(x, y) -> x == y. }.
  impl @{Same(a)}: Same([a]) {
    same = \(left, right) -> case (left, right) {
      | ([], []) -> True
      | ([x | xs], [y | ys]) -> if Same::same x y then Same::same xs ys else False
      | _ -> False
    }.
  }.
  check = \(x, y) -> Same::same x y.
  (check [[1]] [[1]], check [[1]] [[2]]).
  ```

  Expect `(True, False)`. Missing `Same(Bool)` must fail at the use site.
  Also test a custom `Convert(s, t, a, b) determines (s -> a, t -> b)` class
  with a Text-to-List implementation, expected output `[Int]`, and explicit
  numeric result context. Verify a dependency conflict even where the complete
  heads do not overlap, and rejection of uncovered dependent variables.

- [ ] Run binding/runtime suites and confirm failures concern unsupported new
      semantics, not fixture/import mistakes.
- [ ] Freshen each instance's variables, unify the entire head against the
      obligation, apply dependency improvement, and solve prerequisites under the
      strict size/occurrence rules in the RFC. Reject syntactic overlap independently
      of prerequisites and available runtime values. Check bodies under declared
      prerequisites; instantiate method-local variables separately on each use.
- [ ] Pass selected/deferred evidence through analyzed binders and callables.
      Cover stored/partial methods, expected-result methods, empty collections,
      returned closures, and recursive dictionaries. Assert distinguishable results
      for competing test types so an incorrect instance cannot accidentally pass.
      Run `binding-signature-coherence-spec`, `runtime-semantics-spec`,
      `recursive-bindings-spec`, and `haskell-typeclass-contracts-spec`; commit.

### Task 3: Ordinary method values, superclass evidence, and defaults

**Files:** `src/Jazz/Compiler/ModuleResolver.hs`,
`ModuleResolver/Names.hs`, `CoreIdentity.hs`, `TypeInference.hs`,
`TypeInference/Capabilities.hs`, `TypeInference/ImplChecking.hs`,
`Runtime/ScopePlan.hs`, and the binding/runtime/purity suites.

**Interfaces:** A plain method value, `Class::method`, and qualified alias
spelling resolve to the same `CapabilityMethodKey`. Evidence from Task 2
supports superclass projection and omitted-method default bodies.

- [ ] Change the generic helper above to call plain `same`; add stored aliases
      and local shadowing cases. Add a class with `same` and default `different`,
      and a subclass that calls a superclass method. Execute default and overridden
      methods with different results; reject missing methods and superclass cycles.
- [ ] Run binding/runtime and `purity-semantics-spec` tests to establish the
      new cases' failures.
- [ ] Publish class methods into the normal local value environment and resolve
      them before inference. Follow current lexical rules and reject same-scope
      duplicate values. Resolve defaults through the implementation's evidence,
      with supplied methods overriding defaults. Derive superclass evidence from
      its declared graph, never from coincidental same-spelled facts.
- [ ] Confirm class-method aliases preserve bang-name purity checks. Run the
      three focused suites and `source-ranges-spec`; update public capability and
      grammar documentation for this completed behavior and commit.

### Task 4: Module method visibility and transitive instance environments

**Files:** `src/Jazz/Compiler/ModuleExports.hs`, `ModuleInterface.hs`,
`ModuleResolver.hs`, `ModuleResolver/Imports.hs`, `ModuleAnalysis.hs`,
`Parser/ModuleDeclaration.hs`,
`test/Jazz/Compiler/Modules/Loader/CapabilitiesTests.hs`,
`Loader/AliasClassTests.hs`, `ModuleExportsSpec.hs`,
`ModulePipelineContractSpec.hs`, and corresponding hosted module syntax owners.

**Interfaces:** Separate source-accessible method exports from the dependency
instance environment in the existing module interface. Retain supporting hidden
type/class metadata without making those names importable. Use original
declaration identities in both views.

- [ ] Add an A -> B -> C graph: A exports a class/method; B declares an
      implementation; C imports B with no values and uses A's method. Expect
      successful dispatch. Repeated aliases must deduplicate the same instance;
      an additional overlapping instance must fail regardless of import order.
- [ ] Add `class C`, `class C(..)`, selected-method, and method-value-only
      export/import fixtures. Assert that all three method spellings respect the
      same visibility, hidden supporting names remain inaccessible, exported
      generic functions receive caller evidence, and concrete exported bindings
      retain defining-module evidence. Existing re-export rejection stays covered.
- [ ] Implement the selectors and interface changes with focused module/loader
      tests. Update older explicit class exports that expose methods to `C(..)`.
      Run `module-exports-spec`, `module-resolution-spec`, `loader-spec`,
      `module-pipeline-contract-spec`, and the canonical/hosted module suites.
- [ ] Complete the core queue child only after Tasks 1-4 work end to end and
      the frontmatter verification commands pass. Publish matching public contract
      changes, promote the library child from Tasks 5-6, and commit closeout.

### Task 5: Bundled capabilities and requested-output mapping

**Files:** `jazz/stdlib/Prelude.jz`, all collection/Text modules,
new `jazz/stdlib/Reduce.jz`, `src/Jazz/Compiler/BuiltinCatalog.hs`,
`PreludeContract.hs`, `Prelude.hs`, `TypeInference/Capabilities.hs`,
`test/Jazz/Compiler/Stdlib/{LinearCollectionsTests,OrderedCollectionsTests,TextTests,FoundationsTests}.hs`,
`test/Jazz/Repository/AuditSpec.hs`, and new `docs/standard-library/reduce.md`.

**Interfaces:** Implement exactly the classes, method signatures, initial
instance families, and mapping matrix in the RFC. Mappable has four ordered
parameters with source/target element dependencies; Reducible has collection
and element parameters. Map's public implementation is a class method; private
list operations remain internal only where needed.

- [ ] Add representative source programs in modules with `import Mapping.`,
      one unannotated `convert` helper, and explicit result bindings:

  ```jazz
  convert = \(f, values) -> map f values.
  codes :: [Int].
  codes = convert (\(character) -> 1) "abc".
  text :: Text.
  text = convert (\(character) -> character) ['a', 'b'].
  ```

  Expect `codes = [1, 1, 1]` and `text = "ab"`. Module fixtures additionally
  require Queue-to-List, Set-to-List, List-to-Set with duplicate results,
  NonEmpty-to-NonEmpty, Maybe absence, Result error preservation, and maps that
  preserve keys while changing value types. Reject non-Char Text output,
  absent output ordering for Set, ambiguous targets, and incompatible
  functional dependencies. Cover a custom collection absent from the stdlib.

- [ ] Implement ordinary Jazz instance bodies using existing traversal and
      construction functions; share a helper only where concrete repeated code
      warrants one. Place cross-module instance families in an explicit-import
      `Mapping` module to avoid library dependency cycles; list-to-list and existing
      scalar classes remain available with the Prelude. Add `Mapping.jz` and
      `docs/standard-library/mapping.md` to the authored inventory. The RFC's matrix
      is exercised with `import Mapping.`; no source-name or type-name special cases
      belong in the solver.
- [ ] Add the Equatable/Comparable names and generic constraints, respecting
      primitive operation semantics. Add Reducible, Combinable, Empty, and Reduce's
      safe no-seed `reduce`. Preserve FIFO/key/scalar traversal and documented costs.
- [ ] Run `stdlib-spec`, `prelude-loading-spec`, `builtin-catalog-spec`,
      `binding-signature-coherence-spec`, `runtime-semantics-spec`, and `loader-spec`.
      Inspect empty/default behavior explicitly and commit the new library surface.

### Task 6: Public rename, consumers, and final verification

**Files:** Every source/export named in
`2026-09-13-stdlib-api-renames.csv`; `jazz/compiler/`,
`test/fixtures/stdlib/`, Haskell-embedded Jazz fixtures, `programs/`,
`examples/`, `docs/standard-library/`, public examples and signatures,
`jazz/stdlib/README.md`, `scripts/check-stdlib-api-docs.py`,
`scripts/test-check-stdlib-api-docs.py`, and editor grammar owners affected by
the new contextual syntax.

**Interfaces:** The CSV maps existing exports and preserves their argument
orders. Generic Prelude methods remain distinct from specialized module values.
The two class renames are recorded separately in the RFC.

- [ ] Reconcile the CSV with live exports before editing; new exports from Task
      5 already use final names. First migrate public definitions/signatures and
      imports, then references by resolved module ownership. Do not perform a blind
      global replacement of common names such as map, empty, or compare.
- [ ] Preserve current specialized argument orders; use `import List as List`
      and equivalent qualified imports to avoid collisions. Add explicit intended
      result types where the generic map's target is otherwise ambiguous. Update
      overloaded builtin map handling and all hardcoded public capability-name
      inventories together with their consumers.
- [ ] Run `stdlib-spec`, `repository-audit-spec`, the complete retained hosted
      parser/core suites, and the API-doc checker. Assert old public prefix names
      are absent from active exports and consumer code, allowing historical records
      and migration documentation. Add no compiler workaround unless a focused
      source case demonstrates a real required compatibility issue.
- [ ] Run the complete frontmatter verification commands in pinned shells.
      Record each command/result and any explicit new waiver. Full-scale parser
      suites are included by default because grammar changed. No performance claim
      follows from functional tests.
- [ ] Review the full RFC acceptance matrix against observed behavior. Update
      shipped status and public API docs, close the library queue child, refresh
      curation/blocker state, and commit the completed migration.

## Verification environment

Use `/nix/var/nix/profiles/default/bin/nix --extra-experimental-features
'nix-command flakes' develop --command ...` when Nix is absent from PATH.
Use the quality shell selected by the repository for the Haskell quality gate.
No compiler tests are claimed by this planning change. Run RFC, authority,
queue, docs, and whitespace checks for the design documents themselves.

Design validation on 2026-09-13: the complete `scripts/check-docs.sh` gate
passed in the pinned `.#docs` shell, including RFC, authority, and queue
validation. All 183 rename inventory entries were checked against the current
module exports and signatures, with no duplicate destination names per module.
`git diff --check` passed. Compiler build and runtime tests await implementation.

## Design review record

- Approved: behavior-based naming, generic inference and implementations,
  higher-kinded support, Set/Text mapping, and context-selected outputs.
- Concrete new choices in RFC 0019: `determines` syntax, method export selectors,
  transitive instance visibility, `Empty`, and the exact initial mapping matrix.
- Implementation is not promoted while those detailed choices are proposed.
