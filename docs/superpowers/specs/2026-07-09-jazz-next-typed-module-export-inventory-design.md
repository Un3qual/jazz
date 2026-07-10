# Jazz-Next Typed Module Export Inventory Design

## Status

Approved by the maintainer for implementation planning on `2026-07-09`.

## Goal

Replace parallel resolver, compiler-interface, and runtime export selection for
values, constructors, types, and classes with one typed module export inventory
while preserving the current public import syntax, visibility rules, and
diagnostic codes.

The first implementation child is
`JN-MODULE-TYPED-EXPORT-INVENTORY-001`.

## Context

The active `jazz-next` module pipeline already supports:

- bare imports with unqualified visibility;
- explicit symbol-list imports;
- alias-qualified value and constructor lookup;
- non-aliased class imports with class-qualified method dispatch;
- selective hiding of class facts and impl bodies; and
- module-scoped type, capability, and runtime metadata.

The implementation currently represents those exports differently at each
stage. `ModuleResolver.hs` carries separate maps for values, constructors,
data types, and classes. `ModuleInterface.hs` uses `ModuleExport` for value
bindings while keeping data and capability payloads in separate fields.
`ModuleCompiler.hs` then reconstructs selection rules independently when it
filters a dependency interface.

That split makes the module boundary harder to reason about and has allowed
the normative module documentation to lag behind implemented class-import
behavior. A typed inventory gives the resolver and compiler one namespace-aware
model without introducing a second mutable export manifest.

## User-Facing Import Contract

### Non-Aliased imports

`import Lib::Facts.` exposes every supported unqualified export from the
module, including every class declared by that module.

`import Lib::Facts (Eq).` exposes `Eq` as one capability unit. The imported
payload includes:

- the class declaration and arity;
- its method signatures;
- its generated equality marker, when present;
- matching concrete impl facts; and
- matching concrete impl method bodies.

Selecting one class does not expose unrelated classes or their impl payloads.
Impl declarations do not become separately named exports.

### Alias imports

`import Lib::Facts as Facts.` retains the current alias behavior. It exposes
supported values and constructors through `Facts::name`, but exposes no class
capabilities. It neither enables bare `Eq::equals` nor introduces syntax such
as `Facts::Eq::equals`.

### Transitive visibility

Importing a class does not re-export it. A module interface exports only
declarations owned by that module. Imported capability facts may still be
retained internally when an exported binding or locally declared impl body
depends on them, but that retained metadata is not a public class export.

### Explicit selector compatibility

Explicit symbol-list validation remains behavior-compatible:

- values, constructors, and classes are selector-eligible;
- type-only names do not become newly selector-eligible; and
- once a selector name is valid, selection retains every typed inventory entry
  with that text, preserving same-named entries in different namespaces.

This keeps current constructor/type pairs coherent without widening the
language to new type-only import behavior.

## Architecture

### Shared export model

Create `jazz-next/src/JazzNext/Compiler/ModuleExports.hs` as the sole owner of
the shared export model:

```haskell
data ModuleExport = ModuleExport
  { moduleExportNamespace :: NameNamespace,
    moduleExportName :: Text
  }

newtype ModuleExportInventory =
  ModuleExportInventory (Set ModuleExport)
```

`ModuleExport` moves out of `ModuleInterface.hs`; callers import it from the
new module. The inventory remains opaque outside `ModuleExports.hs` so
selection and namespace rules cannot be reimplemented with ad hoc set
operations.

The module provides focused helpers for:

- constructing an inventory from typed entries;
- listing entries or names in one namespace;
- listing selector-eligible names;
- selecting all entries whose names appear in an explicit import list;
- filtering entries for bare, explicit, and alias import modes; and
- testing typed membership.

Selector eligibility is a policy function over `NameNamespace`; it is not a
field stored redundantly on each export.

### Resolver flow

`ModuleResolver.hs` builds one inventory for every parsed module from its
top-level declarations:

- top-level bindings contribute `ValueNamespace` entries;
- data constructors contribute `ConstructorNamespace` entries;
- data declarations contribute `TypeNamespace` entries; and
- class declarations contribute `CapabilityNamespace` entries.

Signatures, impl declarations, module declarations, and import declarations do
not contribute independently named exports.

Name resolution, import validation, collision detection, explicit-list
visibility, and class-origin resolution query this inventory rather than
parallel `Set Text` maps. Import syntax remains text-based, so collision
diagnostics continue to compare selector-visible names even though the
underlying inventory preserves namespace identity.

### Compiled interface flow

`ModuleInterface.hs` continues to own the payload maps needed after inference:

- exported value types;
- declared data type metadata;
- local class facts and method signatures;
- local concrete impl facts and method bodies; and
- runtime hints.

It exposes a derived inventory function that computes typed export entries
from those payloads. The inventory is not stored as another field, preventing
the names and payloads from drifting apart.

`ModuleCompiler.hs` uses the derived inventory and the shared selection helpers
when it imports a dependency interface. It filters each payload map according
to the selected typed entries. Selecting a capability delegates matching impl
fact and method filtering to the selected capability names, preserving the
existing class-as-unit behavior.

`ModuleRuntime.hs` uses the same selected inventory for runtime module exports.
Ordinary runtime cells follow selected value/constructor entries; qualified
method cells follow the selected capability names and remain non-selectable as
independent `Class::method` import symbols.

Imported data metadata remains available under qualified internal keys for
type identity and runtime support, even when its type name is not user-visible.
User-visible type-name resolution still obeys the selected inventory.

### Alias boundary

The shared inventory represents all namespaces, but alias filtering returns
only value and constructor entries for current `Alias::name` lookup. Type
metadata remains internal for identity and runtime support; capability entries
are not returned. The refactor does not add multi-segment qualified names or
module-qualified class dispatch.

## Compatibility and Diagnostics

The child must preserve these observable rules:

- two different non-aliased imports exposing the same selector-visible name
  fail with `E4008`, including class/class and class/value collisions;
- repeating the same module import remains idempotent;
- local value shadowing keeps its current precedence;
- a local class colliding with a visible imported class remains `E1004`;
- missing explicit symbols remain `E4007`;
- hidden explicit and alias-only value references remain `E4011` and `E4012`;
- unknown or hidden class qualifiers retain current `E4013` behavior;
- missing alias members remain `E4014`; and
- missing resolver inventory data remains the internal `E4010` failure.

This refactor introduces no new diagnostic codes. Diagnostic subjects, primary
spans, related spans, importer paths, and dependency paths remain stable.

## Scope

### Included

- the typed inventory module and Cabal registration;
- resolver migration from parallel export maps;
- module-interface inventory derivation;
- compiler import filtering through shared selection helpers;
- focused unit, resolver, and loader regressions;
- normative module import documentation alignment; and
- feature-status and execution metadata alignment.

### Excluded

- new import or export syntax;
- explicit module export declarations or re-exports;
- alias-qualified class or method syntax;
- separately selectable impl declarations;
- orphan or overlap policy changes;
- default methods or superclasses;
- dictionary passing or optimization;
- package and module-root semantics; and
- changes under legacy `jazz-hs/` or `jazz2/`.

## Implementation Boundary

The implementation plan will use the child id
`JN-MODULE-TYPED-EXPORT-INVENTORY-001` and modify only these product and test
surfaces:

- create `jazz-next/src/JazzNext/Compiler/ModuleExports.hs`;
- create `jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs`;
- modify `jazz-next/src/JazzNext/Compiler/ModuleInterface.hs`;
- modify `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`;
- modify `jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs`;
- modify `jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs` to import the moved
  export identity type from its single owner;
- modify `jazz-next/jazz-next.cabal`;
- modify `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`;
- modify
  `jazz-next/test/JazzNext/Compiler/Modules/Loader/CapabilitiesTests.hs`;
- modify
  `jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs`;
- modify `jazz-next/scripts/test-warning-config.sh` to include the focused
  inventory suite;
- modify `docs/spec/modules/04-qualified-imports-and-binding.md`;
- modify `docs/feature-status.md`; and
- align `docs/execution/queue.md`, `docs/execution/blocker-contracts.md`, and
  the linked child plan when the row is promoted.

No unrelated parser, runtime-dispatch, type-solver, primitive, operator, or
legacy compiler files belong in the child.

## Test Design

### Inventory unit coverage

`ModuleExportsSpec.hs` will prove that:

- entries with the same text remain distinct across namespaces;
- selector-eligible names exclude type-only entries;
- selecting an eligible name retains all same-text typed entries;
- alias filtering excludes capability entries; and
- bare filtering retains the supported unqualified inventory.

### Resolver coverage

`ModuleResolutionSpec.hs` will prove that:

- bare class imports participate in visible-name resolution;
- explicit class selectors validate and resolve;
- type-only explicit selectors remain invalid;
- class/class and class/value collisions retain `E4008` metadata;
- repeated imports remain idempotent; and
- alias imports do not create visible capability origins.

### Compiler and loader coverage

`CapabilitiesTests.hs` will prove that:

- selecting a class imports its signatures and matching impl bodies;
- unrelated class and impl payloads remain hidden;
- alias-only classes remain unavailable;
- imported classes are not transitively re-exported; and
- class-qualified runtime dispatch still uses the originating module's data,
  type hints, and retained dependencies.

## Verification

Run the focused suites first:

```sh
jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
```

Then run the full repository gates:

```sh
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

## Acceptance Criteria

The child is complete when:

1. Resolver and compiler import selection use the shared typed inventory.
2. Parallel namespace export maps no longer encode independent selection
   policy.
3. Public import behavior and existing diagnostic codes remain unchanged.
4. Explicit class imports carry exactly the selected class capability payload.
5. Alias imports and transitive module boundaries do not expose capabilities.
6. Focused and full verification pass.
7. The queue row, child plan, blocker contracts, module specification, and
   feature status agree on the completed behavior.
