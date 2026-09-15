---
id: JN-MODULE-API-COMPOSITION-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-09-15
plan_section: Implementation
target_paths:
  - src/Jazz/Compiler/ModuleExports.hs
  - src/Jazz/Compiler/ModuleGraph.hs
  - src/Jazz/Compiler/ModuleImportScope.hs
  - src/Jazz/Compiler/ModuleResolver.hs
  - src/Jazz/Compiler/ModuleResolver/Names.hs
  - src/Jazz/Compiler/ModuleResolver/Imports.hs
  - src/Jazz/Compiler/ModuleInterface.hs
  - src/Jazz/Compiler/ModuleAnalysis.hs
  - src/Jazz/Compiler/TypeInference.hs
  - src/Jazz/Compiler/Name.hs
  - src/Jazz/Compiler/Parser.hs
  - src/Jazz/Compiler/Parser/ModuleDeclaration.hs
  - src/Jazz/Compiler/Parser/Declaration.hs
  - src/Jazz/Compiler/Parser/Context.hs
  - src/Jazz/Compiler/Parser/Expression.hs
  - src/Jazz/Compiler/Parser/Operator.hs
  - src/Jazz/Compiler/Parser/Lower.hs
  - jazz.cabal
  - test/Jazz/Compiler/Modules/ModuleExportsSpec.hs
  - test/Jazz/Compiler/Modules/ModuleResolutionSpec.hs
  - test/Jazz/Compiler/Modules/ModulePipelineContractSpec.hs
  - test/Jazz/Compiler/Modules/Loader/VisibilityTests.hs
  - test/Jazz/Compiler/Modules/Loader/AliasClassTests.hs
  - test/Jazz/Compiler/Modules/Loader/DiagnosticsTests.hs
  - test/Jazz/Compiler/Modules/Loader/OperatorsTests.hs
  - test/Jazz/Compiler/Parser/ModuleImportParserSpec.hs
  - test/Jazz/Compiler/Parser/OperatorFixitySpec.hs
  - test/Jazz/Compiler/Parser/OperatorInvalidSyntaxSpec.hs
  - test/Jazz/Compiler/Parser/OperatorSectionSpec.hs
  - test/Jazz/Compiler/GeneratedInvariantsSpec.hs
  - test/Jazz/Repository/AuditSpec.hs
  - examples/modules/src/Example/OperatorLibrary.jz
  - examples/modules/src/Example/OperatorAPI.jz
  - examples/modules/src/Example/OperatorConsumer.jz
  - scripts/example-cases.tsv
  - docs/language/modules.md
  - docs/language/operators.md
  - docs/reference/module-resolution.md
  - docs/reference/expression-grammar.md
  - docs/reference/diagnostics.md
  - docs/project/status.md
  - .codex/execution/queue.md
  - .codex/execution/blocker-contracts.md
  - .codex/plans/2026-09-15-module-reexports-and-operator-transport.md
verification:
  - cabal test module-import-parser-spec operator-fixity-spec operator-invalid-syntax-spec operator-section-spec module-resolution-spec module-exports-spec module-pipeline-contract-spec loader-spec --jobs=1 --test-show-details=failures
  - JAZZ_CABAL_JOBS=1 bash scripts/ci/haskell-quality.sh
  - JAZZ_CABAL_JOBS=1 bash scripts/ci/main-functional.sh
  - bash scripts/check-execution-queue.sh
  - python3 scripts/check-rfcs.py .
  - git diff --check
deliverable: Explicit identity-preserving re-exports and custom operator transport through module facades, with qualified uses and current public documentation.
---

# Module re-exports and operator transport implementation plan

> **For agentic workers:** Use `superpowers:executing-plans` to implement the
> approved plan task by task. The default is inline execution with focused
> commits; this plan does not require parallel agents.

**Goal:** A library can publish ordinary declarations and custom operators
through explicit facade exports without changing their original identities.

**Architecture:** Extend the existing resolver and typed publication path.
Discover imports from tokens before the single body parse, resolve dependencies
first, and supply imported fixity to the existing precedence parser. Runtime
execution continues using original binding cells.

**Tech stack:** Pinned GHC 9.14.1, Haskell2010 with per-module extensions,
`containers`, Megaparsec, Cabal/Nix.

**Spec:** [RFC 0021](../../rfcs/accepted/0021-module-reexports-and-operator-transport.md).

**Status:** In progress. The maintainer authorized RFC 0021 implementation on
2026-09-15. Execute the complete contract and verify before closing this plan.

> **2026-09-15 amendment:** Accepted RFC 0022 retires the hosted compiler and
> parity/full-scale obligations in the original plan. Task 4 is retired; the
> Haskell feature design and its acceptance gate remain unchanged. This removal
> branch does not include the separate RFC 0021 implementation.

## Global constraints

- Explicit named re-exports; no new public spelling introduced by an export.
- Existing operator characters, precedence range, tiers, and grouping rules.
- Qualified and unqualified infix uses, function values, and both sections.
- Omitted export lists keep imported declarations and custom operators private.
- Preserve original value references, nominal type/class identities, schemes,
  default methods, implementation IDs, and runtime cells.
- Resolve diamond imports by original identity; reject distinct conflicting
  declarations. Preserve transitive instances independently of visible names.
- Discover imports without grouping expressions; parse each body once.
- Reuse the acyclic module graph; no export fixpoint, second interpreter,
  forwarding wrappers, generic pass framework, or new library dependency.
- Implement and verify one frontend in Haskell under RFC 0022.
- Use advanced Haskell when it removes concrete duplication or invalid states.
  Keep extension choices local and the existing toolchain unchanged.
- Update public behavior docs with implementation, not with this proposal.

## Source findings and design

### Existing owners

| Owner                                                  | Current behavior                                                                                                   | Required change                                                                                       |
| ------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------- |
| `Parser/ModuleDeclaration.hs`                          | Parses imports and export selectors; scans import aliases.                                                         | Shared header/import discovery and qualified/operator selectors.                                      |
| `Parser.hs`, `Parser/Context.hs`, `Parser/Operator.hs` | Parse with built-ins plus source-ordered local declarations; discard declaration metadata after parsing.           | Accept imported fixity, retain local fixity as a parser result, distinguish qualified operator names. |
| `ModuleResolver.hs`                                    | `parseModuleDetails` fully parses before `visitModule` follows imports; exports come only from local declarations. | Discover dependencies first; select public targets from local declarations and validated imports.     |
| `ModuleResolver/Names.hs`                              | Resolves imported names using the immediate provider; builds public references from local statements.              | Resolve through original public targets, including types/classes and re-exported operator binders.    |
| `ModuleImportScope.hs`, `ModuleResolver/Imports.hs`    | Store provider path/span and namespace selection.                                                                  | Retain provider location for diagnostics while comparing original export identities.                  |
| `ModuleInterface.hs`, `TypeInference.hs`               | Publish local typed bindings and reachable supporting definitions.                                                 | Assemble selected imported bindings alongside local declarations without re-inference.                |
| `ModuleAnalysis.hs`                                    | Constructs imported nominal names from the immediate dependency.                                                   | Use selected target identity; keep authored aliases only for lookup/diagnostics.                      |
| `ModuleRuntime.hs`                                     | Publishes and imports existing cells by `ResolvedReference`.                                                       | Consume the complete interface; no forwarding cells or wrapper evaluation.                            |

### Extend the existing public boundary

No new export carrier, target sum type, header record, operator-name ADT, or
phase is needed. Reuse `ModuleExportInventory`, `ModuleExportSelector`,
`ResolvedName`, `ResolvedReference`, `ModuleValueBinding`, `OperatorTable`,
`SurfaceStatement`, and the current phase-indexed module records.

The genuinely missing facts are original public-name ownership and exported
fixity. Add them to existing owners:

```haskell
-- Additional fields on ResolvedModuleFacts:
resolvedModuleExportNames :: Map ModuleExport ResolvedName
resolvedModuleOperators :: [OperatorInfo]

-- Additional field on ModuleInterface:
interfacePublicNames :: Map ModuleExport ResolvedName
```

The public-name map connects a selectable name to the original published
`ResolvedName` in its namespace. It is necessary for type/class re-exports:
`ResolvedNameOrigin` currently reconstructs the immediate provider, which would
incorrectly give a facade ownership of the declaration. It also gives value
lookup the original key for the existing `resolvedPublicReferencesState` map;
that map already owns original `ResolvedReference` values. Do not create another
value-reference registry. Preserve original references in existing
`ModuleValueBinding` records and reuse them at runtime.

Resolve selector names once, then derive the public-name map and the existing
inventory/constructor/class relationships from that selection. Keep import
provider paths/spans for diagnostics, but compare original published names for
collision identity. The original defining module has one final binding per
public namespace/name; ordinary local aliases remain distinct declarations.
Repeated paths to that same export are compatible. No renaming is introduced.

Preserve every validated unqualified import selection in `ValidatedImportScope`
by changing the existing per-name origin field to:

```haskell
importScopeNames :: Map NameNamespace (Map Text (NonEmpty BindingOrigin))
```

The existing `BindingOrigin` already carries the provider and import span.
Append compatible origins in source order instead of overwriting one with
`Map.union`. Adapt `importedNameOrigins` and its consumers to resolve those
providers through the public-name maps; validation requires one original target
per namespace/name. `dependencyImportViews` groups all retained origins by
provider/span, preserving each import's selected inventory and relationships.
Unqualified grouped exports merge the visible constructor subsets for that
original type; qualified exports use only their alias's inventory. This replaces
the lossy field without adding a parallel import-selection cache or record type.

Extend the existing resolver's inventory cache to hold `ResolvedModuleFacts`
by module path, projecting inventories/names/operators as needed. Do not add
three new parallel caches. The existing resolved module sequence, DFS cycle
handling, `CoreModule phase`, `ModuleFactsAt`, and nominal roles remain.
Fixity is a frontend fact and is not copied into `ModuleInterface` or runtime
objects; the typed interface needs only selected visibility, original names,
existing typed bindings, type definitions, and capabilities.

Keep `resolvedPublicReferences` for defining declarations, including explicitly
exported operator bindings. Facades resolve to those existing keys through the
public-name map rather than inserting facade-owned reference aliases.
`Names` and `importSelectedInterface` must both look up the same original name,
including class methods and constructor witness names. Build the typed public
interface from local inference entries and selected dependency entries; copy
original schemes/references without re-inference. Supporting private types and
instances remain separate from selectable names. Keep the existing type
reachability traversal and runtime-cell publication functions.

`publishModuleInterface` must use the selected public-name keys when checking
availability. Its current `declaredInterfaceInventory` rebuilds type/class
names with `renderName`; doing that to an imported `A::Box` would incorrectly
drop a facade's public `Box` selector. Check that each selected name resolves to
the original definition, rather than silently intersecting mismatched spellings.
Use that same map to seed type reachability: take the original target of every
selected `TypeNamespace` entry, then add the existing roots from public bindings
and capability schemes. The current root filter also uses `renderName`; leaving
it unchanged would discard an abstract re-export that no public value mentions.
Keep traversal and hidden-constructor visibility unchanged.
Constructor ownership and class-method sets remain keyed by the public names
within each inventory; their nominal definitions retain original identities.

### Discover imports; parse once

Extend `Parser/ModuleDeclaration.hs`, which already owns import parsing and
import-alias scanning. Share its module-prefix/export-selector and import
grammar with a bounded token walk. Return existing `SSModule` / `SSImport`
surface statements in source order; there is no new `ModuleHeader` data type.
The module declaration is optional. The scanner allocates no core nodes and
never parses an expression.

This walk replaces `collectImportAliasesUntilEnd` /
`collectImportAliasesUntilBrace` and their private token-scanning helpers.
Derive aliases from the discovered imports with the existing
`registerImportAliases Set.empty` projection. Seed `parserKnownAliases` once;
statement-list parsing consumes that context instead of scanning tokens again
or re-registering each import. Keep dependency loading in the resolver.

The scanner skips balanced non-import statement tokens and recognizes imports
only at the root statement depth of the optional module wrapper. Parentheses,
lists, nested braces, and period-containing selectors must not change that
boundary. Strings/comments are already owned by the lexer. Imports may occur
after uses. Nested imports are rejected by the full parser and never loaded by
discovery. The ordinary parser still parses the full token stream and allocates
IDs in its current order; it owns body errors.

`visitModule` becomes:

```text
load source -> tokenize -> discover SSModule/SSImport declarations
  -> visit dependencies in existing sorted DFS order
  -> seed ParserContext with discovered aliases and selected imported operators
  -> parse full token stream once with that context
  -> lower, validate import uses, resolve local declarations
  -> select public inventory, original names, and exported fixity
```

The early operator selection and later name checks must share the import
selection/collision helpers. The early result supplies parse-time visibility;
the later pass adds checks requiring body references, not a second operator
precedence policy. Error order follows the RFC's explicit phase ordering.

Add one parser entrypoint using existing types:

```haskell
parseSurfaceProgramTokensWithContext ::
  ParserContext -> [Token] -> Either Diagnostic (SurfaceExpr, [OperatorInfo])
```

Callers start from `initialParserContext`, setting only its existing alias set
and operator table. The supplied statement context remains `TopLevelContext`.
The module wrapper changes it to `ModuleBodyContext` while preserving both
visibility fields; nested expression blocks inherit them as they do today.
No new parser-input record or discovery-complete flag is needed.

The second result contains only operator declarations authored in this source
unit. Obtain them from the final parser table minus the declared keys in the
supplied context's operator table; imported redefinitions are rejected, so this
difference is unambiguous.
Do not keep a second authored-declaration table. Add the small projection inside
`Parser/Operator.hs`, which owns the table. Exported module metadata filters this
list to explicitly selected operators; it contains no built-ins. Existing
standalone entrypoints run the same discovery walk once, seed aliases with the
default operator table, and project `fst`; they do not load dependencies. The
resolver supplies its already-discovered aliases directly to the context-aware
entrypoint. Keep parser failures in the current detailed error path.

Return the final `ParserContext` alongside statements from the existing
statement-list and module-body callbacks. Both end-of-input and closing-brace
paths must return it; the module branch of `parseStatementParser` must forward
the body's context instead of restoring the incoming context. Expression-block
callers project the statements and keep their enclosing context. The public
entrypoint can then read the final table for wrapped and unwrapped sources.
No new result record or mutable parser state is needed.

Operator declarations already pass through the parser: retain their fixity
there, not in a second scanner. Reuse the existing `Text` operator payloads and
lookup keys. A qualified spelling is the validated adjacent `Alias::%%` form;
use existing `splitQualifiedIdentifierText` / qualified-name helpers to resolve
it. The qualifier is an alias, never a full module path. Preserve alias and
operator token spans at diagnostic sites. This needs neither a new
`OperatorName` ADT nor changed surface/core expression constructors.

At resolution, use the existing `SourceName` cases and ordinary call/section
normalization. Unqualified custom operators still use their current generated
binder names locally; imported references use the original public-name entry
and its existing resolved reference. Built-in mappings apply only to
unqualified built-in spellings. Alias-qualified operators refer only to
explicitly exported custom operators. Fixity never reaches checked expressions.

Extend selector parsing and one normalization/rendering boundary for qualified
names and parenthesized operators. Replace the ordinary selector's bare `Text`
payload with the existing `LocatedModuleExportName`; grouped type selectors
already retain their locations. Keep authored names and spans through lowering,
and qualify them with `qualifyModuleExportSelectorSpans`. Duplicate syntax still
compares rendered selector keys, not spans. Semantic export conflicts use the
later selector as primary and the earlier selector as related; no token rescans
or spelling-keyed span table is needed. Convert operator names to the current
encoded value key only at inventory lookup. Avoid a parallel selector hierarchy
or expanding `NameNamespace`.

### Alternatives considered

| Approach                                                          | Decision                                                                                                                             |
| ----------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| Header/import token discovery followed by one body parse          | Selected: reuses the token stream, parser, DFS, and nominal identities.                                                              |
| Parse with guessed fixities, then parse again                     | Rejected: provisional grouping can affect syntax failures and duplicates the expensive body parse.                                   |
| Preserve unresolved infix chains and resolve fixity after parsing | Viable, but adds a new expression representation and touches cases, guards, sections, and diagnostics for no other current consumer. |
| Facade wrapper bindings or re-created nominal declarations        | Rejected: changes binding identity, sections/effects, constructor compatibility, or class evidence.                                  |
| Generalized export graph or fixpoint framework                    | Rejected: the current acyclic dependency graph already orders all re-exports.                                                        |

### Haskell feature research

Reviewed official GHC documentation on 2026-09-15 against the pinned 9.14.1
toolchain in `flake.nix`. Some linked development-guide pages display 9.15;
the selected features predate 9.14.1. No compiler upgrade is proposed.

| Feature                                                  | Concrete decision and benefit                                                                                                                                                                 | Primary source                                                                                                                                                                                                |
| -------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `DataKinds` and closed `TypeFamilies`                    | Retain `CoreModule phase` / `ModuleFactsAt`; resolved export targets belong only in resolved/analyzed facts, preventing their use in raw lowered modules. No new phase hierarchy.             | [Datatype promotion](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/exts/data_kinds.html), [type families](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html)             |
| `RoleAnnotations`                                        | Retain the existing nominal roles on module/program carriers so coercion cannot bypass the phase distinction.                                                                                 | [Roles](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/roles.html)                                                                                                                                   |
| `DerivingStrategies`, `DeriveGeneric`, `DeriveAnyClass`  | Retain explicit `stock` Eq/Ord/Show/Generic and `anyclass` NFData as existing records gain fields; this avoids handwritten forcing.                                                           | [Deriving strategies](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_strategies.html), [extra derived classes](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_extra.html) |
| `DeriveFunctor` / `DeriveFoldable` / `DeriveTraversable` | Reuse the existing derived traversals on `Name user`. Header statements and original-name maps do not need new parameterized record types just to use deriving.                               | [Derived traversals](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_extra.html#deriving-traversable-instances)                                                                              |
| `DerivingVia`                                            | Considered and not selected. Export merging can fail with source diagnostics; a coercible Monoid wrapper would hide those semantics. Existing explicit `Either Diagnostic` folds are clearer. | [Deriving via](https://downloads.haskell.org/ghc/9.14.1-rc3/docs/users_guide/exts/deriving_via.html)                                                                                                          |

Reuse existing name and reference constructors instead of adding an
`ExportTarget` sum or GADT. Keep ordinary `Map`, `Set`, `traverse`, and `foldM`
for selection. Enable `NamedFieldPuns` or
`LambdaCase` locally if they shorten new code without obscuring ownership.
No Template Haskell, lenses, extensible effects, or general-purpose selector
typeclasses are needed for this change.

## Implementation

Each task includes behavioral checks and a focused commit. Intermediate commits
belong to one feature branch; the release boundary is the completed RFC contract.
Abbreviated compiler paths in Tasks 1-3 are relative to `src/Jazz/Compiler/`;
abbreviated test paths are relative to `test/Jazz/Compiler/`.

### Task 1: Preserve original public identity through explicit re-exports

**Files:** modify existing compiler files
`ModuleExports.hs`, `ModuleGraph.hs`, `ModuleImportScope.hs`, `ModuleInterface.hs`,
`ModuleResolver.hs`, `ModuleResolver/Imports.hs`, `ModuleResolver/Names.hs`,
`ModuleAnalysis.hs`, `TypeInference.hs`, `jazz.cabal` under their existing owners.
Modify `Parser/ModuleDeclaration.hs` for qualified named selectors and
`ModuleExports.hs` / `Parser/Lower.hs` to retain and qualify selector spans.
Tests belong in
`test/Jazz/Compiler/Modules/ModuleExportsSpec.hs`, `ModuleResolutionSpec.hs`,
`ModulePipelineContractSpec.hs`, and `Modules/Loader/VisibilityTests.hs` /
`AliasClassTests.hs`. Paths without a prefix in this paragraph are under
`src/Jazz/Compiler/` except `jazz.cabal`.

**Consumes:** existing local inventories, dependency interfaces, and validated
import views. **Produces:** original-name fields on existing resolved facts and
the typed interface, plus the current inventories and binding references.

- [ ] Add a three-module value fixture: A exports `answer = 42`, B exports
      `value A::answer` through an alias, C imports B and evaluates `answer`.
      Expect `42`, and assert the public binding reference is identical through A
      and B. Run `loader-spec` and observe the current export rejection first.
- [ ] Add a fixture with `data Box(a) = Box a` and a generic class method.
      Re-export `type A::Box(..)` and `class A::Equal`; consume constructors,
      patterns, `B::Equal::equal`, constraints, and an impl head through the facade.
      Compare direct-plus-facade imports to facade-only execution. Expected type
      identities and selected implementation IDs are the originals.
- [ ] Resolve export selectors after imports and local declarations are known.
      Select by namespace; build inventory, targets, and relationship metadata in
      one operation. Coalesce identical targets and reject distinct collisions.
- [ ] Replace the per-name `BindingOrigin` with `NonEmpty BindingOrigin` in the
      existing import scope. Preserve every validated selection when deriving
      dependency views; merge constructor visibility by original type identity
      for unqualified selectors and retain separate alias views.
- [ ] Replace immediate-provider nominal-name construction in `Names` and
      `importSelectedInterface` with target lookup. Keep provider spans for errors.
      Include re-export selectors in external-use accounting so an exported value
      is retained even when no body expression references it.
- [ ] Assemble typed publication from local typed declarations and selected
      dependency entries. Keep original schemes/references, reachable private type
      definitions, class defaults, and transitive implementations. Reuse the existing
      defining-declaration reference map; do not insert facade-owned references.
- [ ] Seed type reachability from original targets of selected public type
      names. Re-export only abstract `Box(a)` through a facade, with no exported
      values, constructors, or instances referencing it. A consumer signature
      using `API::Box(Int)` must check, and the typed interface must retain the
      original definition and parameter kind while keeping constructors hidden.
- [ ] Retain ordinary selector locations using `LocatedModuleExportName` and
      qualify them during lowering. Test `value Left::answer` versus
      `value Right::answer` selecting distinct declarations: `E4015` points to
      the later selector and relates the earlier one. Pair valid
      `value Left::answer` with unavailable `type Left::answer` and verify the
      error points to the type selector.
- [ ] Cover selected visible constructors, private constructor
      rejection, method-only exports, empty facades carrying instances, and same-text
      names across namespaces. Expected invalid selectors use `E4015` and point to
      the selector. Do not expose hidden metadata as public names.
- [ ] Run the four module suites plus `loader-spec` and `module-import-parser-spec`;
      inspect `git diff --check`, then commit `Support identity-preserving explicit module re-exports`.

### Task 2: Discover imports and retain parser-owned fixity

**Files:** modify existing compiler files
`Parser.hs`, `Parser/ModuleDeclaration.hs`, `Parser/Declaration.hs`,
`Parser/Context.hs`, `Parser/Expression.hs`, `Parser/Operator.hs`,
`ModuleResolver.hs`, and `jazz.cabal`.
Tests: existing `ModuleImportParserSpec.hs`, `OperatorFixitySpec.hs`,
`ModuleResolutionSpec.hs`, and `Modules/Loader/DiagnosticsTests.hs`.

**Consumes:** source tokens and source-ordered imports. **Produces:**
existing `SSModule`/`SSImport` statements for discovery and the
`parseSurfaceProgramTokensWithContext` entrypoint specified above.
Existing entrypoints discover aliases once and use the default operator table.

- [ ] Add discovery cases with a late import, no module wrapper, nested braces,
      tuple/list expressions, strings/comments containing `import`, and constructor
      export groups containing `..`. Only real module-scope imports may be loaded.
      Compare discovered import declarations/spans with the ordinary parser on the
      valid fixtures. Malformed bodies stay the full parser's responsibility.
- [ ] Factor the shared module-prefix and import grammar. Implement the balanced
      token walk without parsing expression precedence, allocating core nodes, or
      duplicating an expression AST. Preserve source-order import metadata.
- [ ] Replace the old alias token walkers and their parser call sites with alias
      projection from discovery into `ParserContext`. Reuse the same discovery
      path in standalone entrypoints; the resolver's parse must not rediscover
      imports. Reuse existing late-alias and signature-disambiguation fixtures to
      compare standalone and supplied-context parsing, including nested uses
      before a later import in wrapped and unwrapped sources.
- [ ] Supply imported operators to the existing parser context and retain the
      metadata already produced for local declarations. A local declaration must
      still precede use; duplicate local declarations keep their current failures.
      The module-body context in `parseStatementParser` must inherit the supplied
      aliases and table instead of resetting them. Nested expression blocks inherit
      lookup visibility but still reject operator declarations.
- [ ] Return the final context through statement-list and module-body callbacks;
      expression-block callers project statements. Verify wrapped and unwrapped
      sources return their authored fixities, exclude supplied imported fixities,
      and retain declarations after a nested expression block.
- [ ] Change `visitModule` to the discovery/dependency/body order above. Keep
      sorted DFS and cycle diagnostics. Use common selection helpers for parse-time
      operator imports and later body-dependent visibility checks.
- [ ] Lock the intentional mixed-error order with one missing-dependency plus
      malformed-body fixture. Retain existing single-error messages and spans.
      Add a loader callback count assertion proving each module source is loaded
      once; do not add AST-shape snapshots of the scanner implementation.
- [ ] Run `module-import-parser-spec`, `operator-fixity-spec`,
      `operator-invalid-syntax-spec`, `module-resolution-spec`, and `loader-spec`;
      commit `Discover module imports before parsing bodies`.

### Task 3: Transport custom operators through ordinary public interfaces

**Files:** modify `Name.hs`, `Parser/Expression.hs`,
`Parser/Declaration.hs`, `Parser/ModuleDeclaration.hs`, `Parser/Operator.hs`,
`Parser/Lower.hs`, `ModuleExports.hs`,
`ModuleResolver.hs`, `ModuleResolver/Names.hs`, `ModuleResolver/Imports.hs`,
`ModuleInterface.hs`, and `TypeInference.hs`. Keep existing operator expression
constructors and visitors; extend shared name decoding at the boundaries.
Tests: `Modules/Loader/OperatorsTests.hs`, `ModuleExportsSpec.hs`, and the
existing parser operator suites.

**Consumes:** selected public targets and parser fixity results.
**Produces:** explicit operator exports and fixity in existing resolved module
facts, and ordinary resolved function calls in every notation.

- [ ] Add the complete RFC example to `OperatorsTests.hs`, using
      `runModuleGraphWithPrelude`, `lookupSourceIn`, and the existing fixture map.
      Expected checks are concrete:

  ```haskell
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "facade operator output"
    (Just "(7, 7, 7, 7, 7, 7, 7, 7, 14, 42)") (runOutput result)
  ```

- [ ] Parse `(%%)` in imports and `(%%)` / `(Ops::%%)` in export selectors.
      Use value namespace and existing internal operator binder encoding. Preserve
      original spelling and precise alias/operator spans for errors.
- [ ] Extend existing operator payload/table lookup to validated `Alias::%%`
      spellings. Support all four qualified forms from the RFC. Resolve them using the same
      function lookup and existing section templates as unqualified operators.
      Retain RFC 0020 built-in name mapping and ordinary type/evidence dispatch.
- [ ] Require a defining declaration and executable binding for public operators;
      imported selectors copy both the original target and its fixity. Add explicit
      selection to local typed publication; omitted lists keep generated operator
      binders private. Declaration-only export fails with `E4015`.
      Publish fixity under the defining unqualified spelling; alias imports prefix
      only the consuming parser's lookup key. A facade never stores an alias as the
      operator's defining spelling.
- [ ] Reject imported-operator rebinding, signatures, and local fixity collisions.
      Permit a same-spelled local operator when the dependency is alias-only.
      Reject distinct unqualified origins regardless of matching fixity. For these
      cases use the current collision/module-syntax family and verify the authored
      operator spelling and related source locations.
- [ ] With the RFC's left-associative provider, test
      `10 API::%% 3 API::%% 1` gives `6`. In a separate provider fixture declaring
      `operator %% precedence 6 right.`, the same qualified subtraction chain
      gives `8`. Import both providers under different aliases to check their
      distinct fixities without redeclaring either imported operator. Also test
      a non-associative chain rejection and an alias-only symbol's unqualified
      rejection.
- [ ] Test section capture using the existing observable host harness or a
      captured failing operand, plus generic/constrained operator aliases and
      explicit type application. Preserve the existing hidden-operator tests.
- [ ] Run the parser/operator and module suites from frontmatter; commit
      `Transport custom operators through imports and re-exports`.

### Task 4: Retired hosted parity work

The original task mirrored selector/operator syntax and canonical lowering in
the hosted frontend. Accepted RFC 0022 removes that implementation and its
exclusive comparison and scale tests. No replacement task is required.

### Task 5: Combined conformance, public documentation, and closeout

**Files:** extend `Modules/Loader/VisibilityTests.hs`, `OperatorsTests.hs`,
`AliasClassTests.hs`, `DiagnosticsTests.hs`, and `ModulePipelineContractSpec.hs`;
add `examples/modules/src/Example/OperatorLibrary.jz`, `OperatorAPI.jz`, and
`OperatorConsumer.jz`. Update `scripts/example-cases.tsv`,
`test/Jazz/Repository/AuditSpec.hs`, `docs/language/modules.md`,
`docs/language/operators.md`, `docs/reference/module-resolution.md`,
`docs/reference/expression-grammar.md`, `docs/reference/diagnostics.md`,
`docs/project/status.md`, and the active queue/contract/plan.

**Consumes:** complete Haskell execution.
**Produces:** verified public feature, executable examples, and closed dispatcher.

- [ ] Test direct A plus facade B plus facade C as a diamond, then reverse
      import order. Values, constructors, class methods, defaults, and operators
      must resolve to the original identities. Distinct same-spelled definitions
      fail deterministically. Include a facade-cycle rejection.
- [ ] Extend the diamond fixture with A exposing `T(C1, C2)` and B exposing
      the same `T(C1)`. A consumer importing both and exporting `type T(..)`
      must retain both constructors in either import order. An alias-qualified
      export through B must retain only `C1`, including when A is also imported.
- [ ] Test a facade exposing only an operator whose function uses private
      helpers, nominal types, and capability evidence. Its dependency must be
      retained without exposing those helper names. Keep dependencies' top-level
      expressions unexecuted and entry-module effects unchanged.
- [ ] Add one meaningful generated property in
      `test/Jazz/Compiler/GeneratedInvariantsSpec.hs`:
      acyclic chains/diamonds of facade edges carrying one original binding preserve
      target identity and value under import-order permutations; replacing one leaf
      with a distinct same-spelled declaration is rejected. Do not generate a full
      language grammar or test every implementation helper.
- [ ] Register and execute the three-module example; update documentation for
      exact syntax, visibility, default privacy, imported fixity, collisions, and
      the deliberate error-order change.
- [ ] Run focused suites once after the last relevant change, then the Haskell
      quality gate and authoritative `scripts/ci/main-functional.sh` serially in
      the pinned Nix shell with `--jobs=1`.
- [ ] Confirm queue/docs checks and `git diff --check`. Commit the examples and
      contract updates, record the verified implementation commit, and remove the
      completed candidate/ready row. Do not leave completed work dispatchable.

### Contract coverage

| RFC requirement                                                                       | Implementation and evidence                                                      |
| ------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------- |
| Explicit named, qualified, grouped-constructor, and class re-exports                  | Task 1 parser/selector work and loader fixtures.                                 |
| Original identity, schemes, cells, private support metadata, and transitive instances | Task 1 publication/lookup changes; Task 5 diamond and private-dependency cases.  |
| Default privacy, duplicate paths, namespace separation, and distinct collisions       | Tasks 1 and 3 negative cases; Task 5 import permutations and generated property. |
| All operator notations, fixity, local restrictions, and capture semantics             | Tasks 2-3 parser/lookup/section cases.                                           |
| Late imports, one body parse, cycle handling, and structured error order              | Task 2 discovery/diagnostic cases; Task 5 facade-cycle case.                     |
| Executable public contract and dispatcher closeout                                    | Task 5 registered example, public docs, quality/main gates, and queue update.    |

## Design review and promotion

Before implementation, review the RFC's exact selector/qualified-operator syntax,
explicit operator export rule, immutable imported fixity, and error-order change.
After acceptance, move RFC 0021 to `rfcs/accepted/`, update both indexes and
this link, set this plan to `ready` / `autonomous_ready: yes`, and promote its
single candidate with exactly matching frontmatter. Both module and operator
umbrella blockers refer to this one batch; do not create duplicate work rows.

Self-review must trace every RFC section to Tasks 1-5 and verify all referenced
paths and suite names. Review typed publication and parser-order changes as the
highest-risk boundaries. A successful docs check proves the plan's structure,
not that the proposed language behavior has been implemented or tested.
