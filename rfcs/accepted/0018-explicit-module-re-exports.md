# RFC 0018: Explicit module re-exports

Status: Accepted
Date: 2026-09-10
Supersedes: RFC 0017's prohibition on explicit class re-exports and associated transitive publication.

RFC 0017's omitted-list behavior, alias qualification, and other class rules
remain in force.

## Decision

Allow namespace-qualified module-header selectors to publish declarations from
explicit unqualified imports, preserving their original identities. Reuse the
existing `value`, `type`, `constructor`, and `class` selector syntax.

Accepted by the maintainer on 2026-09-10 and implemented in the Haskell
compiler, including the public module contract and executable facade example.
Hosted/bootstrap work and native execution remain deferred.

For example, given a dependency that publicly exports `makeBox`, the type
`Box` with constructor `Box`, and the class `Equal`:

```jazz
module Library::API (value makeBox, type Box(..), class Equal) {
  import Library::Model.
}
```

A client can import `Library::API` normally, selectively, or through an alias.
`API::Box` denotes the original type and constructor, and
`API::Equal::equal` dispatches through the original class. The facade creates
no new declarations or implementation identities.

## Context

Jazz already has explicit typed export lists and alias-qualified classes, but
libraries cannot group public declarations behind a facade. Consumers must
depend on each declaring module's layout. Value wrappers cannot forward
nominal types, constructors, or classes without changing their meaning.

The current boundary assumes that public declarations are locally owned:
the resolver validates header selectors before visiting dependencies; module
interfaces and binder inventories collect local declarations; import analysis
assigns the immediate dependency as their origin; runtime publication looks up
local declaration cells. Re-exports must change these assumptions together.
Broadening an export-name set alone would misidentify nominal declarations or
publish an interface with no corresponding runtime binding.

## Selection and visibility

- An omitted export list continues to export only owned declarations. `()`
  continues to export nothing. Importing a dependency alone never publishes it.
- Bare selectors such as `(Box)` retain their owned-only compatibility meaning,
  including selection across owned namespaces. Re-exporting requires a typed
  selector; adding an import cannot widen an existing bare export.
- A typed selector resolves in its named namespace. An owned declaration takes
  precedence over imported declarations in that namespace; otherwise lookup
  uses explicit unqualified imports and their existing symbol-list filtering.
  Import collisions are still validated before export selection.
- Ambient Prelude declarations and metadata retained privately for imported
  values are not eligible. An explicit ordinary import is required to forward
  any library declaration, including a declaration from a Prelude module.
- Imports through aliases do not satisfy an unqualified header selector. This
  batch introduces no alias-qualified header selectors, renaming, body-level
  export statements, or whole-module export shortcut. The facade's clients may
  still use all existing import forms.
- The exported spelling is unchanged. A facade can select a subset of a direct
  dependency's public interface, including that dependency's explicit
  re-exports. It cannot reach a private or unselected declaration by looking
  through the dependency to an earlier module.
- Selecting a type or class alone does not publish same-spelled declarations
  from other namespaces. Existing value/constructor expression-name collision
  rules remain in force; typed headers do not make ambiguous imports legal.

### Types and constructors

`type T` publishes the original type abstractly. `type T(..)` also publishes all
constructors of that type currently visible through explicit unqualified
imports. It never recovers constructors hidden by any intervening export or
import list. If none are visible, it is equivalent to `type T`.

`type T(C1, C2)` requires each named constructor to be visible and to belong to
the selected original type. A local same-spelled constructor cannot be used as
an imported type's constructor. `constructor C` publishes just the constructor
binding and its existing type metadata, without making the owner type publicly
nameable. Constructor ownership must survive even when the owner type is not
itself exported, so later validation cannot infer ownership from spelling.

When identical type or class identities arrive through multiple public paths,
their selected public members can be combined. Only already visible members
participate; private metadata never supplies additional public constructors.

## Identity, collisions, and execution

Each public entry records both its exported namespace/spelling and its original
declaration identity. Consumers resolve through that identity instead of
manufacturing a new owner from the facade module's path. Type schemes,
constructors, class references, binder IDs, and implementation evidence must
agree on the original owner throughout a chain of facades.

Repeated routes to the same declaration are idempotent. Importing a declaration
directly and through a facade, or through two facades, does not produce a new
binding collision solely because the immediate module paths differ. Distinct
declarations with conflicting source names remain errors under the existing
namespace rules. Aliases can distinguish those declarations for consumers.
Overlapping selectors for the same declaration remain idempotent as today.

At runtime, forwarding publishes the original binding cell and its captured
environment. It must not synthesize a wrapper, reevaluate the declaration, or
lose hidden dependencies needed by a closure. Dependencies still execute once
per module graph, suppress dependency top-level expressions, and preserve the
existing dependency-first order. Re-exports introduce no new graph edges:
every forwarded declaration must come through an explicit import. Existing
cycle rejection remains unchanged.

## Classes and implementation evidence

An explicit `class C` re-export forwards the selected direct dependency's
public payload for the original class: method signatures, concrete impl facts,
method bodies and evidence candidates. This includes evidence that dependency
has itself explicitly forwarded for the class. It also publishes impls owned
by the facade for that class under the existing class-attached impl policy.
For example, a facade may implement an imported class for its own concrete ADT
and publish that impl alongside the explicitly re-exported class.

Only payloads associated with that class and actually made visible by the
facade's explicit imports participate. Importing a helper value or carrying
private type/evidence metadata does not make its hidden class or impls public.
Omitting `class C` does not transitively publish imported class evidence.
Owned-class publication and existing hidden evidence transport for exported
values retain their current behavior.

Evidence is deduplicated by original implementation identity across repeated
paths. Separate implementations that violate the current duplicate or
ambiguity rules still fail; no first-import or first-facade winner is allowed.
All existing method contexts must work through facades: direct calls, stored
and partially applied methods, explicit instantiation, constrained functions,
and impl heads. This adds no separate impl imports, overlap/orphan relaxation,
default methods, superclasses, or effect-system behavior.

## Diagnostics

Retain `E4015` for an invalid export selector, including a private, missing,
alias-only, ambient-only, wrong-namespace, or hidden-constructor selection.
Report the failing selector or constructor at its source location. A wrong
constructor owner should identify the selected type and the mismatched
constructor. Do not fall back to an ambient or same-spelled declaration after
typed selection fails.

Retain the existing import collision and alias diagnostic families for import
errors. Diagnostics should distinguish the immediate import route from the
original declaring module when that explains a collision. Validation and
error ordering must be deterministic. Syntactic validation remains in parsing;
dependency-sensitive export validation moves after dependency discovery.

## Implementation boundaries and acceptance evidence

Extend the current shared module boundary rather than adding a second resolver
or independently rediscovering origins at runtime. The concrete owners are
`src/Jazz/Compiler/ModuleExports.hs`, `ModuleGraph.hs`, `ModuleResolver.hs`,
`ModuleResolver/Imports.hs`, `ModuleResolver/Names.hs`, `ModuleInterface.hs`,
`ModuleAnalysis.hs`, `ModuleCompiler.hs`, and `ModuleRuntime.hs`; change
`TypeInference.hs` only as needed for interface assembly and identity transport.
Public export metadata must distinguish public selection from private semantic
dependencies. Preserve original binder IDs and implementation owners.

Acceptance requires behavior coverage for:

- A value and closure forwarded through two facades, including a hidden helper
  and an observable check that dependency work is not duplicated.
- Direct and facade imports of one ADT, constructor patterns, abstract exports,
  partial constructor groups, constructor-only exports, and hidden-constructor
  rejection across multiple hops.
- Class methods and constrained functions through aliased and unqualified
  facade imports, plus facade-owned impls for an imported class.
- Direct-plus-facade and diamond imports preserving type/class identity and
  deduplicating original evidence; genuinely distinct impls still failing.
- Private, symbol-filtered, alias-only and ambient-only selections; wrong
  namespaces and constructor owners; local shadowing; same-spelled distinct
  declarations; unchanged bare selectors, omitted lists, and empty lists.
- Compile/runtime interface agreement, stable error locations, unchanged
  dependency-expression suppression and cycle errors.

Use `module-exports-spec`, `module-resolution-spec`,
`module-pipeline-contract-spec`, and `loader-spec` as the focused suites, then
run the supported Haskell quality gate and repository documentation/queue
checks. When implementation lands, update `docs/language/modules.md`,
`docs/reference/module-resolution.md`, the export-selector grammar,
`docs/language/capabilities.md`, diagnostics where needed, and shipped status.
Add an executable facade example to the existing checked module examples.

Bootstrap, hosted module semantics, and native execution remain deferred. The
existing header grammar does not change; no new hosted syntax or parity work
is authorized. On acceptance, stage the implementation under `.codex/plans/`
and promote the candidate in `.codex/execution/queue.md`.

## Alternatives

Alias-qualified header selectors would permit a facade to choose among
conflicting imports without opening their names locally. They require a new
grammar and output-name contract. Existing unqualified imports suffice for
this batch; qualified selectors and renaming can be designed separately.

A whole-module shortcut or automatic re-export of all imports would reduce
enumeration but let dependency growth widen a facade's API. Explicit typed
selectors provide a deliberate public boundary and preserve current defaults.

Value-only forwarding would avoid some evidence work but leave facades unable
to publish the types and classes needed to use those values. Forwarding the
existing four declaration namespaces forms one usable module boundary.

## Consequences

Libraries can publish a stable facade without copying definitions or changing
nominal identity. The cost is explicit origin and constructor-ownership
transport across resolver, compiler, and runtime publication, plus class
evidence forwarding only where a class is explicitly re-exported.

Previously valid header lists keep their public selection. Some previously
invalid typed selectors and duplicate import routes become valid. No migration
is required for existing programs; documentation must explain the new meaning
of typed selectors for imported names and the unchanged owned-only defaults.
Packages, cross-module operators, new standard-library APIs, broader class
features, and purity changes are outside this decision.
