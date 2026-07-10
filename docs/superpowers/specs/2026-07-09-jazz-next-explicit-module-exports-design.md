# Jazz-Next Explicit Module Export Lists Design

## Status

Approved by the maintainer for implementation planning on `2026-07-09`.

## Goal

Add an optional module-header export allowlist to active `jazz-next` so module
authors can define a stable public API while keeping unlisted declarations
available for local inference and runtime evaluation.

This is the highest-leverage follow-up to the typed module export inventory.
It establishes encapsulation before later cross-module operator, abstraction,
effect, package, or re-export work.

The first implementation child will use the id
`JN-MODULE-EXPLICIT-EXPORT-LIST-001`.

## Surface Syntax

The canonical form is an optional parenthesized name list after the declared
module path:

```jazz
module Lib::Maybe (Maybe, Just, Nothing, mapMaybe) {
  data Maybe a = Just a | Nothing.
  privateHelper = 1.
  mapMaybe = \(f) -> \(value) -> value.
}
```

The list is an allowlist. Only listed owned declarations are public.

An omitted list preserves the current export-all contract:

```jazz
module Lib::Maybe {
  value = 1.
}
```

An empty list is valid and publishes no declarations:

```jazz
module App::Internal () {
  helper = 1.
}
```

Files that omit a module declaration continue to derive their module path from
the resolved source path and retain export-all behavior. There is no standalone
export declaration outside the module header.

## Export Selection Semantics

Module export lists select owned declarations by exact source text across the
typed inventory. A selected name retains every matching namespace entry:

- top-level bindings contribute value entries;
- data constructors contribute constructor entries;
- data declaration names contribute type entries; and
- class declarations contribute capability entries.

Unlike explicit import lists, module export lists may select type-only names.
The existing import selector policy remains unchanged: a type-only name is not
valid as an explicit import selector by itself.

This permits precise data API boundaries:

- exporting a type without its constructors creates an opaque type;
- exporting constructors separately permits construction without making the
  type name available in source type positions; and
- a same-text type/constructor pair is selected as one textual API name while
  remaining distinct in the inventory.

Exporting a class publishes the class fact, method signatures, and matching
impl facts and method bodies as one capability payload. Impl declarations are
not independently named exports.

Only declarations owned by the module are eligible. Imported values,
constructors, types, and classes cannot be listed, so this child does not add
re-export behavior.

## Compatibility

The feature is opt-in. Existing declared modules without an export list and
source-path-derived modules remain export-all and preserve current resolver,
compiler, runtime, and diagnostic behavior.

The entry module obeys the same publication policy, but its full body is still
compiled and evaluated. Its terminal expression may use private declarations
and still produce the program result.

`private` is descriptive terminology only. This design does not add a keyword
or declaration modifier; an owned declaration is private when it is absent
from an explicit export list.

## Architecture

### Parser and lowering

`SurfaceStatement.SSModule` gains an optional export-name list. The module
declaration parser accepts either `{` immediately after the module path or a
parenthesized list followed by `{`.

The parser permits `()` and rejects duplicate names at the duplicate token.
Lowering preserves the optional list on `CoreModule` for auditability while
continuing to remove module/import declarations from the executable body.

### Resolver inventories

`ModuleResolver` derives two inventories with different responsibilities:

1. The local declaration inventory contains every owned declaration and is
   used for resolving names inside the module.
2. The public export inventory is the local inventory filtered by the optional
   module export list and is the only inventory published to dependents.

These are not parallel namespace maps. They are two values of the shared
`ModuleExportInventory` type representing distinct local and public boundaries.

Resolved dependency state stores public inventories only. Local name and class
origin checks use the local inventory. Import validation, collision detection,
and dependency member resolution use dependency public inventories.

`ResolvedModule` carries the validated public inventory so later stages do not
reparse or reconstruct the source export policy.

### Compiled module boundary

`ModuleInterface` remains the full inferred payload for the module. It may
retain private values, data metadata, class metadata, impl facts, method
bodies, and runtime hints needed to validate and execute the module.

`CompiledModule` reaches the validated public inventory through its existing
`compiledResolvedModule` field. It does not store a second copy. This separates
publication policy from retained compiler support metadata without creating a
new drift-prone manifest.

When compiling a dependent module, `ModuleCompiler` filters the dependency's
full interface through both:

1. the dependency's public inventory; and
2. the consuming import's bare, explicit, or alias selection policy.

The full interface retains data metadata under compiler-owned qualified keys so
public value and constructor types can always be rebased. The resolver's public
inventory prevents an unlisted type name from becoming source-visible; retained
metadata is never a public type export by itself.

### Runtime publication

The runtime evaluates the full compiled module expression. Private helpers are
therefore available to local statements and may be captured by exported
closures.

Runtime publication uses the compiled module's public inventory. Only public
value and constructor cells are inserted into `RuntimeModule` exports. Class
method cells follow public capability entries and remain attached to the class
rather than becoming independently selectable symbols.

Import-time runtime filtering still applies the consuming import mode after the
module publication boundary.

## Diagnostics

Malformed module export syntax and duplicate export names use the existing
parser diagnostic code `E0001`. Duplicate diagnostics identify the repeated
name and use the repeated token span.

The resolver adds `E4015` for a module export name that does not match an owned
declaration. The diagnostic:

- uses the missing export name as its subject;
- uses the module declaration span as its primary span;
- names the declared module path and source file; and
- lists the available owned declaration names deterministically.

An imported name with no same-text owned declaration therefore produces
`E4015`; it is not treated as a re-export.

Downstream behavior keeps existing codes:

- explicitly importing an unexported name remains `E4007`;
- requesting a missing alias member remains `E4014`;
- import collisions remain `E4008`; and
- hidden class qualifiers retain current `E4013` behavior.

This child adds no other diagnostic code.

## Test Design

### Parser coverage

Extend `ModuleImportParserSpec.hs` to prove:

- populated module export lists parse and lower;
- `()` is accepted;
- omitted lists remain unchanged;
- duplicates report `E0001` at the repeated token; and
- malformed comma, parenthesis, and missing-body forms fail deterministically.

### Inventory and resolver coverage

Extend `ModuleExportsSpec.hs` and `ModuleResolutionSpec.hs` to prove:

- module export selectors include value, constructor, type, and capability
  namespaces;
- same-text namespace entries remain selected together;
- local resolution sees private declarations;
- dependency resolution sees only public entries;
- unknown and imported-only export names report `E4015` with metadata;
- explicit imports of private names report `E4007`; and
- modules without lists remain export-all.

### Compiler and runtime coverage

Extend `ModulePipelineContractSpec.hs` and focused loader suites to prove:

- compiled modules retain the full inferred interface and a distinct public
  inventory;
- a public closure can capture and execute a private helper;
- private runtime cells are not published;
- private data metadata can support public signatures without exposing the
  private type name;
- exporting a type without constructors creates an opaque API;
- constructors can be exported separately;
- public classes retain matching method and impl payloads;
- private classes and impl payloads do not leak; and
- entry-module output remains unchanged by a restrictive public list.

### Repository verification

Run the focused parser, inventory, resolver, pipeline, and loader suites first,
then run:

```sh
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

## Scope

### Included

- optional module-header export allowlists;
- empty and populated allowlists;
- value, constructor, type, and capability selection;
- opaque types and separately exported constructors;
- local-versus-public inventory ownership;
- public inventory propagation through resolved and compiled modules;
- compiler/runtime publication filtering;
- deterministic parser and `E4015` diagnostics;
- focused regression coverage; and
- normative module and execution metadata alignment.

### Excluded

- re-exports or forwarding imported declarations;
- wildcard, `(..)`, type-family, or constructor-group export shorthand;
- body-level export declarations;
- `private` or `public` declaration modifiers;
- cross-module user-defined operator exports;
- package manifests, package roots, or package aliases;
- default methods, superclasses, dictionaries, or new impl policy;
- effect typing or cross-module purity graphs; and
- changes under legacy `jazz-hs/` or `jazz2/`.

## Acceptance Criteria

The implementation child is complete when:

1. The approved header syntax parses with omitted, empty, and populated lists.
2. Local compilation and runtime evaluation retain unlisted declarations.
3. Resolver, compiler import selection, and runtime publication expose only the
   validated public inventory.
4. Values, constructors, types, and classes follow the specified namespace and
   payload rules.
5. Re-exports remain impossible.
6. `E4015` and parser diagnostics carry deterministic subject/span/context
   evidence.
7. Existing export-all modules and `E4007`-`E4014` behavior remain compatible.
8. Focused suites, the full `jazz-next` harness, queue/docs validators, and
   whitespace checks pass.
